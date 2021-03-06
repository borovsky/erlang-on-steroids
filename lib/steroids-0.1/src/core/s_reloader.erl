%%%
%%% This Library is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU Library General Public License as
%%% published by the Free Software Foundation; either version 3 of the
%%% License, or (at your option) any later version.
%%%
%%% This Library is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% Library General Public License for more details.
%%%
%%% You should have received a copy of the GNU Library General Public
%%% License along with the Gnome Library; see the file COPYING.LIB.  If not,
%%% write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
%%% Boston, MA 02111-1307, USA.
%%%

%%%
%%% @author Alexander Borovsky <partizan@altlinux.ru>
%%% @doc Server, that reloads templates/controllers/models
%%% @end
%%%
-module(s_reloader).

-behaviour(gen_server).

%% API
-export([start/0, start_link/0, behaviour_info/1]).

-export([load_thing/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% tests exports
-export([terminate/0, timeout_module/1]).

-include_lib("kernel/include/file.hrl").


-define(SERVER, ?MODULE).
-define(RECHECK_TIME, 1).
-define(Million, 1000000).

-inline([update_last_checked/1,
         compile/3,
         update_last_check/2,
         reload_module/2,
         last_reload_time/1,
         last_checked/1]).

%%====================================================================
%% API
%%====================================================================
-spec behaviour_info(atom()) -> 'undefined' | [{atom(), byte()}].
behaviour_info(callbacks) ->
    [{get_real_path, 1}, {compile_and_load, 2}, {get_module_name, 1}];
behaviour_info(_Other) ->
    undefined.

%%
%% @spec start() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%
-spec(start/0 :: () -> {ok, pid()} | ignore | {error, any()}).
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%%
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%
-spec(start_link/0 :: () -> {ok, pid()} | ignore | {error, any()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%
%% @spec load_thing(atom(), string()) -> atom()
%% @doc Starts the server
%% @end
%%
-spec(load_thing/2 :: (atom(), string()) -> atom()).
load_thing(CallbackModule, Path) ->
    {Result, ModuleName} = is_reload_required(CallbackModule, Path),
    ReloadResult = case Result of
                       yes -> reload_module(CallbackModule, Path);
                       not_found -> throw(not_found);
                       last_check_update -> update_last_check(CallbackModule, Path);
                       _ -> ok
                   end,
    case ReloadResult of
        ok -> list_to_atom(ModuleName);
        {error, not_found} -> throw(not_found);
        Other -> throw(Other)
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server
%% @end
%%
-spec(init/1 :: ([]) -> {ok, tuple()}).
init([]) ->
    ets:new(?SERVER, [named_table, protected]),
    {ok, {}}.

%%
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Handling call messages
%%
-spec(handle_call/3 :: (term(), term(), term()) -> any()).
handle_call({load_module, CallbackModule, Path}, _From, State) ->
    Response = server_reload_module(CallbackModule, Path),
    {reply, Response, State};
handle_call(terminate, _From, State) ->
    {stop, shutdown, ok, State};
handle_call({timeout_module, ModuleName}, _From, State) ->
    ets:insert(?SERVER, {{lookup, ModuleName}, {0,0,0}}),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Handling cast messages
%% 
-spec(handle_cast/2 :: (term(), term()) -> any()).
handle_cast({update_last_check, CallbackModule, Path}, State) ->
    server_reload_module(CallbackModule, Path),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% 
-spec(handle_info/2 :: (term(), term()) -> any()).
handle_info(_Info, State) ->
    {noreply, State}.

%% 
%% @spec terminate(Reason, State) -> void()
%% 
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end
%% 
-spec(terminate/2 :: (term(), term()) -> any()).
terminate(_Reason, _State) ->
    ok.

%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end
%%
-spec(code_change/3 :: (term(), term(), term()) ->
             {ok, term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%
%% @spec terminate() -> any()
%% @doc Terminates server. For tests only
%% @private
%% @end
%%
-spec(terminate/0 :: () -> any()).
terminate() ->
    gen_server:call(?SERVER, terminate).

%%
%% @spec timeout_module(string()) -> any()
%% @doc Expire module. Modification time will be checked again. For tests only
%% @private
%% @end
%%
-spec(timeout_module/1 :: (string()) -> any()).
timeout_module(ModuleName) ->
    gen_server:call(?SERVER, {timeout_module, ModuleName}).

%%
%% @spec checked_ago(atom()) -> integer()
%% @doc Returns time from last module check
%%
-spec(checked_ago/1 :: (string()) -> integer() | not_found).
checked_ago(ModuleName) ->
    case ets:lookup(?SERVER, {lookup, ModuleName}) of
        [] -> not_found;
        [{_, Old}] ->
            round(timer:now_diff(erlang:now(), Old) / ?Million)
    end.

%%
%% @spec last_reload_time(atom()) -> tuple()
%% @doc Returns time from last module check
%%
-spec(last_reload_time/1 :: (string()) -> date_time()).
last_reload_time(ModuleName) ->
    case ets:lookup(?SERVER, {reload, ModuleName}) of
        [] -> {{1,1,1},{0,0,0}}; % Really old date
        [{_, ReloadTime}] -> ReloadTime
    end.


%%
%% @spec reload_module(atom(), string()) -> any()
%% @doc Reloads module
%%
-spec(reload_module/2 :: (atom(), string()) -> any()).
reload_module(CallbackModule, Path) ->
    gen_server:call(?SERVER, {load_module, CallbackModule, Path}).

%%
%% @spec is_reload_required(atom(), string()) ->
%%              {yes | no | last_check_update | not_found, string()}
%% @doc Checks, if reload required
%%
-spec(is_reload_required/2 :: (atom(), string()) ->
             {yes | no | last_check_update | not_found, string()}).
is_reload_required(CallbackModule, Path) ->
    ModuleName = apply(CallbackModule, get_module_name, [Path]),
    CheckedAgo = checked_ago(ModuleName),
    Result = case CheckedAgo of
        not_found -> yes;
        _ when CheckedAgo > ?RECHECK_TIME ->
            RealPath = case apply(CallbackModule, get_real_path, [Path]) of
                           not_found -> s_log:error(?MODULE, "Module ~s can't find ~p", 
                                                    [CallbackModule, Path]),
                                        not_found;
                           Str -> Str
            end,
            LastReloadTime = last_reload_time(ModuleName),
            case filelib:last_modified(RealPath) of
                ChangeTime when LastReloadTime < ChangeTime ->
                    yes;
                0 -> not_found;
                _ -> last_check_update
            end;
        _ -> no
    end,
    {Result, ModuleName}.

%%
%% @spec update_last_check(atom(), string()) -> ok
%% @doc Updates last check time for module asynchronius
%%
-spec(update_last_check/2 :: (atom(), string()) -> ok).
update_last_check(CallbackModule, Path) ->
    gen_server:cast(?SERVER, {update_last_check, CallbackModule, Path}),
    ok.

%%
%% @spec compile_and_load(atom(), string(), string()) -> ok | {error, Reason}
%% @doc Compiles and load module
%%
-spec(compile_and_load/3 :: (atom(), string(), string()) -> ok | error).
compile_and_load(CallbackModule, Path, ModuleName) ->
    RealPath = apply(CallbackModule, get_real_path, [Path]),
    filelib:ensure_dir(filename:join(s_conf:get(ebin_dir), "some")),
    s_log:info(?MODULE, "Reloading ~s", [RealPath]),

    case filelib:last_modified(RealPath) of
        0 -> {error, not_found};
        ChangeTime ->     
            case apply(CallbackModule, compile_and_load, [RealPath, list_to_atom(ModuleName)]) of
                ok -> 
                    ets:insert(?SERVER, {{reload, ModuleName}, ChangeTime}),
                    update_last_checked(ModuleName);
                Error -> Error
            end
    end.

%%
%% @spec update_last_checked(string()) -> any()
%% @doc Updated last checked time for module
%%
-spec(update_last_checked/1 :: (string()) -> any()).
update_last_checked(ModuleName) ->
    ets:insert(?SERVER, {{lookup, ModuleName}, erlang:now()}),
    ok.

%%
%% @spec server_reload_module(atom(), string()) -> any()
%% @doc Checks and reload module (server variant)
%%
-spec(server_reload_module/2 :: (atom(), string()) -> ok | error).
server_reload_module(CallbackModule, Path) ->
    {Result, ModuleName} = is_reload_required(CallbackModule, Path),
    case Result of
        yes -> compile_and_load(CallbackModule, Path, ModuleName);
        no -> ok;
        last_check_update -> update_last_checked(ModuleName)
    end.


%%%-------------------------------------------------------------------
%%% @author Alexander Borovsky <partizan@altlinux.ru>
%%% @doc Server, that reloads templates/controllers/models
%%% @end
%%%-------------------------------------------------------------------
-module(s_reloader).

-behaviour(gen_server).

%% API
-export([start/0, start_link/0, behaviour_info/1]).

-export([load_thing/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/file.hrl").


-define(SERVER, ?MODULE).
-define(RECHECK_TIME, 10).
-define(Million, 1000000).

-inline([update_last_checked/1,
         compile/3,
         update_last_check/2,
         reload_module/2,
         get_change_time/1,
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

%%--------------------------------------------------------------------
%% @spec start() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
-spec(start/0 :: () -> {ok, pid()} | ignore | {error, any()}).
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
-spec(start_link/0 :: () -> {ok, pid()} | ignore | {error, any()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


-spec(load_thing/2 :: (atom(), string()) -> atom()).
load_thing(CallbackModule, Path) ->
    {Result, ModuleName} = is_reload_required(CallbackModule, Path),
    case Result of
        yes -> reload_module(CallbackModule, Path);
        last_check_update -> update_last_check(CallbackModule, Path);
        _ -> ok
    end,
    list_to_atom(ModuleName).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server
%% @end
%%--------------------------------------------------------------------
-spec(init/1 :: ([]) -> {ok, tuple()}).
init([]) ->
    ets:new(?SERVER, [named_table, protected]),
    {ok, {}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
-spec(handle_call/3 :: (term(), term(), term()) -> any()).
handle_call({load_module, CallbackModule, Path}, _From, State) ->
    server_reload_module(CallbackModule, Path),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
-spec(handle_cast/2 :: (term(), term()) -> any()).
handle_cast({update_last_check, CallbackModule, Path}, State) ->
    server_reload_module(CallbackModule, Path),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
-spec(handle_info/2 :: (term(), term()) -> any()).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
-spec(terminate/2 :: (term(), term()) -> any()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec(code_change/3 :: (term(), term(), term()) ->
             {ok, term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% @spec last_checked(atom()) -> integer()
%% @doc Returns time from last module check
%%--------------------------------------------------------------------
-spec(last_checked/1 :: (string()) -> integer() | not_found).
last_checked(ModuleName) ->
    {MegaSecNow, SecNow, MicroSecNow} = erlang:now(),
    case ets:lookup(?SERVER, {lookup, ModuleName}) of
        [] -> not_found;
        [{_, {MegaSec, Sec, MicroSec}}] ->
                Diff = (MicroSecNow - MicroSec) + ((SecNow - Sec) + (MegaSecNow - MegaSec) * ?Million) * ?Million,
                Diff div ?Million
    end.

%%--------------------------------------------------------------------
%% @spec last_reload_time(atom()) -> tuple()
%% @doc Returns time from last module check
%%--------------------------------------------------------------------
-spec(last_reload_time/1 :: (string()) -> date_time()).
last_reload_time(ModuleName) ->
    case ets:lookup(?SERVER, {reload, ModuleName}) of
        [] -> {{1,1,1},{0,0,0}}; % Really old date
        [{_, ReloadTime}] -> ReloadTime
    end.

%%--------------------------------------------------------------------
%% @spec get_change_time(string()) -> date_time()
%% @doc Returns time from last module check
%%--------------------------------------------------------------------
-spec(get_change_time/1 :: (string()) -> date_time()).
get_change_time(Path) ->
    case file:read_file_info(Path) of
        {ok, FileInfo} -> FileInfo#file_info.mtime;
        {error, Reason} -> 
            io:format("File not found: ~s~n", [Path]),
            throw({problem_with_file_access, Reason})
    end.

%%--------------------------------------------------------------------
%% @spec reload_module(atom(), string()) -> any()
%% @doc Reloads module
%%--------------------------------------------------------------------
-spec(reload_module/2 :: (atom(), string()) -> any()).
reload_module(CallbackModule, Path) ->
    gen_server:call(?SERVER, {load_module, CallbackModule, Path}).

%%--------------------------------------------------------------------
%% @spec is_reload_required(atom(), string()) ->
%%              {yes | no | last_check_update, string()}
%% @doc Checks, if reload required
%%--------------------------------------------------------------------
-spec(is_reload_required/2 :: (atom(), string()) ->
             {yes | no | last_check_update, string()}).
is_reload_required(CallbackModule, Path) ->
    ModuleName = apply(CallbackModule, get_module_name, [Path]),
    LastChecked = last_checked(ModuleName),
    Result = case LastChecked of
        not_found -> yes;
        _ when LastChecked > ?RECHECK_TIME ->
            RealPath = apply(CallbackModule, get_real_path, [Path]),
            LastReloadTime = last_reload_time(ModuleName),
            ChangeTime = get_change_time(RealPath),
            case get_change_time(RealPath) of
                ChangeTime when LastReloadTime < ChangeTime ->
                    yes;
                _ -> last_check_update
            end;
        _ -> no
    end,
    {Result, ModuleName}.

%%--------------------------------------------------------------------
%% @spec update_last_check(atom(), string()) -> ok
%% @doc Updates last check time for module asynchronius
%%--------------------------------------------------------------------
-spec(update_last_check/2 :: (atom(), string()) -> ok).
update_last_check(CallbackModule, Path) ->
    gen_server:cast(?SERVER, {update_last_check, CallbackModule, Path}),
    ok.

%%--------------------------------------------------------------------
%% @spec compile_and_load(atom(), string(), string()) -> ok
%% @doc Compiles and load module
%%--------------------------------------------------------------------
-spec(compile_and_load/3 :: (atom(), string(), string()) -> ok).
compile_and_load(CallbackModule, Path, ModuleName) ->
    RealPath = apply(CallbackModule, get_real_path, [Path]),
    ChangeTime = get_change_time(RealPath),
    io:format("Compiling: ~s~n", [RealPath]),
    apply(CallbackModule, compile_and_load, [RealPath, list_to_atom(ModuleName)]),
    ets:insert(?SERVER, {{reload, ModuleName}, ChangeTime}),
    update_last_checked(ModuleName),
    ok.

%%--------------------------------------------------------------------
%% @spec update_last_checked(string()) -> any()
%% @doc Updated last checked time for module
%%--------------------------------------------------------------------
-spec(update_last_checked/1 :: (string()) -> any()).
update_last_checked(ModuleName) ->
    ets:insert(?SERVER, {{lookup, ModuleName}, erlang:now()}).

%%--------------------------------------------------------------------
%% @spec server_reload_module(atom(), string()) -> any()
%% @doc Checks and reload module (server variant)
%%--------------------------------------------------------------------
-spec(server_reload_module/2 :: (atom(), string()) -> any()).
server_reload_module(CallbackModule, Path) ->
    {Result, ModuleName} = is_reload_required(CallbackModule, Path),
    case Result of
        yes -> compile_and_load(CallbackModule, Path, ModuleName);
        no -> ok;
        last_check_update -> update_last_checked(ModuleName)
    end.


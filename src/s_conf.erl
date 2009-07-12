%%%-------------------------------------------------------------------
%%% File    : s_conf.erl
%%% Author  : Alexander Borovsky <partizan@altlinux.ru>
%%% Description : 
%%%
%%% Created : 11 Jul 2009 by Alexander Borovsky <partizan@altlinux.ru>
%%%-------------------------------------------------------------------
-module(s_conf).

-behaviour(gen_server).

%% API
-export([start/0, start_link/0, reload_config/0, server_root/0, set_value/2, get_value/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
%% Internal (for tests, etc)
-export([terminate/0]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start() ->
    gen_server:start({global, ?MODULE}, ?MODULE, [],  []).


%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [],  []).

%%
%% @spec reload_config() -> ok
%% @doc Reloads config
%% @end
%%
-spec(reload_config/0 :: () -> ok).
reload_config() ->
    gen_server:call({global, ?MODULE}, reload).

%%
%% @spec server_root() -> ServerRootDir :: string()
%% @doc Returns the path to site root dir
%% @end
%%
-spec(server_root/0 :: () -> string()).	     
server_root() ->
    case application:get_env(site, server_root) of
        undefined     -> 
            ServerRoot = calculate_server_root(),
            application:set_env(site, server_root, ServerRoot),
            ServerRoot;
        {ok, RootDir} -> 
            RootDir
    end.

%%
%% @spec set_value(tuple(), any()) -> ok
%% @doc Sets global config value
%% @end
%%
-spec(set_value/2 :: (tuple(), any()) -> ok).	     
set_value(Key, Value) ->
    gen_server:call({global, ?MODULE}, {set, {Key, Value}}).

%%
%% @spec get_value(atom()) -> not_found | any()
%% 
%% @doc Returns the global configuration value, or not_found atom if
%% configuration not found
%%
%% @end
%%
-spec(get_value/1 :: (atom()) -> not_found | any()).	     
get_value(Key) ->
    case ets:lookup(?MODULE, Key) of
        [] -> not_found;
        [{_Key, Value}] -> Value
        end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    ets:new(?MODULE, [named_table, protected]),
    Data = load_config(),
    ets:insert(?MODULE, Data),
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
handle_call(reload, _From, State) ->
    Data = load_config(),
    ets:insert(?MODULE, Data),
    {reply, ok, State};

handle_call(terminate, _From, State) ->
    {stop, shutdown, ok, State};

handle_call({set, Data}, _From, State) ->
    ets:insert(?MODULE, Data),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ets:delete(?MODULE),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%
%% @spec load_config() -> Config :: list({atom(), any()}))
%% @doc Loads config from file
%% @end
%%
-spec(load_config/0 :: () -> list({atom(), any()})).
load_config() ->
    FileName = filename:join([server_root(), "config", "application.conf"]),
    Defaults = dict:from_list(default_config()),
    Config = dict:from_list(load_config(FileName)),
    FilledConfig = dict:merge(fun(_, V, _) -> V end, Defaults, Config),
    dict:to_list(dict:map(fun enhance_config/2, FilledConfig)).


%%
%% @spec load_config() -> Config :: list({atom(), any()}))
%% @doc Loads config from file
%% @end
%%
-spec(terminate/0 :: () -> ok).
terminate() ->
    gen_server:call({global, ?MODULE}, terminate).

default_config() ->
    [
     {upload_dir, "tmp/uploads"}
    ].

%%
%% @spec load_config(FileName) -> Config :: list({atom(), any()}))
%% @doc Loads config from file
%% @end
%%
-spec(load_config/1 :: (string()) -> list({atom(), any()})).
load_config(FileName) ->
    case file:consult(FileName) of
        {ok, Config} ->
            Config;
        {error, Reason} ->
            io:format("Error loading config: ~s~n", [Reason]),
            []
    end.

%%
%% @spec enhance_config({Key, Value}) -> {Key,
%% @doc Enhances configs. f.e. convert relative path to absolute
%% @end
%%
-spec(enhance_config/2 :: (atom(), any()) -> any()).
enhance_config(upload_dir, ["/" | Path]) ->
    Path;
enhance_config(upload_dir, Path) ->
    filename:join(server_root(), Path);

enhance_config(_Key, Value) ->
    Value.

%%
%% @spec calculate_server_root() -> ServerRootDir :: string()
%% @doc Returns the path to site root dir
%% @end
%%
-spec(calculate_server_root/0 :: () -> string()).	     
calculate_server_root() ->
    ThisModulePath = filename:split(code:which(s_conf)),
    io:format("~p~n", [ThisModulePath]),
    filename:join(lists:sublist(ThisModulePath, length(ThisModulePath)-2)).


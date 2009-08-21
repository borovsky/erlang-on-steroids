%%%
%%% @author  Alexander Borovsky <partizan@altlinux.ru>
%%% @doc Global web-application configuration
%%% @version 0.1
%%%
-module(s_conf).

-behaviour(gen_server).

%% API
-export([start/0, 
         start_link/0, 
         reload_config/0, 
         server_root/0, 
         set/2, 
         get/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
%% Internal (for tests, etc)
-export([terminate/0]).

-define(EXPAND_PATH_VARIABLES, [ebin_dir, views_dir, controller_dir, upload_dir, routes_file]).

%%====================================================================
%% API
%%====================================================================

%% 
%% @spec start() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%% 
-spec(start/0 :: () -> {ok, pid()} | ignore | {error, any()}).
start() ->
    gen_server:start({global, ?MODULE}, ?MODULE, [],  []).

%% 
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%% 
-spec(start_link/0 :: () -> {ok, pid()} | ignore | {error, any()}). 
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
%% @spec set(atom(), any()) -> ok
%% @doc Sets global config value
%% @end
%%
-spec(set/2 :: (atom(), any()) -> ok).	     
set(Key, Value) ->
    gen_server:call({global, ?MODULE}, {set, {Key, Value}}).

%%
%% @spec get(atom()) -> not_found | any()
%% 
%% @doc Returns the global configuration value, or not_found atom if
%% configuration not found
%%
%% @end
%%
-spec(get/1 :: (atom()) -> not_found | any()).	     
get(Key) ->
    case ets:lookup(?MODULE, Key) of
        [] -> not_found;
        [{_Key, Value}] -> Value
        end.

%%====================================================================
%% Internal API
%%====================================================================

%%
%% @doc Terminates server (for tests only)
%% @private
%% @end
%%
-spec(terminate/0 :: () -> ok).
terminate() ->
    gen_server:call({global, ?MODULE}, terminate).

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
-spec(init/1 :: (any()) -> tuple() | ignore).
init(_Args) ->
    io:format("Starting s_conf...~n", []),
    ets:new(?MODULE, [named_table, protected]),
    load_config_internal(),
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
-spec(handle_call/3 :: (tuple() | atom(), pid(), any()) -> tuple()).
handle_call(reload, _From, State) ->
    load_config_internal(),
    {reply, ok, State};

handle_call(terminate, _From, State) ->
    {stop, shutdown, ok, State};

handle_call({set, Data}, _From, State) ->
    ets:insert(?MODULE, Data),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Handling cast messages
%% 
-spec(handle_cast/2 :: (any(), any()) -> tuple()).
handle_cast(_Msg, State) ->
    {noreply, State}.

%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% 
-spec(handle_info/2 :: (any(), any()) -> tuple()).
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
-spec(terminate/2 :: (any(), any()) -> ok).
terminate(_Reason, _State) ->
    ets:delete(?MODULE),
    ok.

%% 
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% 
-spec(code_change/3 :: (any(), any(), any()) -> {ok, any()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%
%% @spec load_config() -> Config :: list({atom(), any()})
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



default_config() ->
    [
     {upload_dir, "tmp/uploads"},
     {controller_dir, "app/controllers"},
     {views_dir, "app/views"},
     {ebin_dir, "app/ebin"},
     {routes_file, "config/routes.conf"},
     {log_file, "logs/development.log"},
     {log_level, info},
     {template_engines, [{s_erlydtl_adapter, ["dtl"]}]}
    ].

%%
%% @spec load_config(FileName) -> Config :: list({atom(), any()})
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
%% @spec enhance_config(Key, Value) -> Value :: any()
%% @doc Enhances configs. f.e. convert relative path to absolute
%% @private
%% @end
%%
-spec(enhance_config/2 :: (atom(), any()) -> any()).
enhance_config(Key, Value) ->
    case lists:member(Key, ?EXPAND_PATH_VARIABLES) of
        true -> case Value of
                    ["/" | _] -> Value;
                    _ -> filename:join(server_root(), Value)
                end;
        false -> Value
    end.

%%
%% @spec calculate_server_root() -> ServerRootDir :: string()
%% @doc Returns the path to site root dir
%% @private
%% @end
%%
-spec(calculate_server_root/0 :: () -> string()).	     
calculate_server_root() ->
    ThisModulePath = filename:split(code:which(s_conf)),
    filename:join(lists:sublist(ThisModulePath, length(ThisModulePath)-4)).

%%
%% @spec apply_config_changes(list({atom(), any()})) -> any()
%% @doc Applies config changes
%% @private
%% @end
%%
-spec(apply_config_changes/1 :: (list({atom(), any()})) -> 
             any()).
apply_config_changes(Config) ->
    lists:foreach(fun apply_line/1, Config).

-spec(apply_line/1 :: (list({atom(), any()})) -> any()).
apply_line({ebin_dir, Path}) ->
    code:add_patha(Path);

apply_line(_) ->
    ok.

load_config_internal() ->
    Data = load_config(),
    ets:insert(?MODULE, Data),
    apply_config_changes(Data).

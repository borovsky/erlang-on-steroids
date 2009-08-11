%%%-------------------------------------------------------------------
%%% @author Alexander Borovsky <partizan@altlinux.ru>
%%% @doc Routing subsystem
%%% @end
%%%-------------------------------------------------------------------
-module(s_routes).

-behaviour(gen_server).

%% API
-define(SERVER, ?MODULE).
-export([start_link/0, parse_request/2]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Exports for tests
-export([compile_routes/1, resolve_route/3]).


-include_lib("kernel/include/file.hrl").
-include("s_internal_types.hrl").

-record(state, {last_reload :: date_time(), routes :: list()}).


%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @spec parse_request(atom(), string()) -> dict()
%% @doc Parses requests and returns controller/action
%%--------------------------------------------------------------------
-spec(parse_request/2 :: (atom(), string()) -> routing_result()).
parse_request(Method, URL) ->
    gen_server:call(?SERVER, {resolve, Method, URL}).


%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
-spec(start_link/0 :: () -> {ok, pid()} | ignore | {error, any()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
-spec(init/1 :: (any()) -> {ok, any()}).
init([]) ->
    {ok, load_routes()}.

%%--------------------------------------------------------------------
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Handling call messages
%%--------------------------------------------------------------------
-spec(handle_call/3 :: (any(), pid(), #state{}) -> tuple()).
handle_call({resolve, Method, Path}, _From, State) ->
    Reply = resolve_route(Method, Path, State#state.routes),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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

%%--------------------------------------------------------------------
%% @spec load_routes() -> #state{}
%% @doc Loads routes from file
%% @private
%% @end
%%--------------------------------------------------------------------
-spec(load_routes/0 :: () -> #state{}).
load_routes() ->
    RoutesFile = s_conf:get(routes_file),
    case file:consult(RoutesFile) of
        {ok, Routes} ->
            ChangeTime = s_utils:get_change_time(RoutesFile),
            State = #state{last_reload = ChangeTime, routes = compile_routes(Routes)},
            garbage_collect(),
            State;
        {error, Reason} ->
            throw({error_loading_routes, Reason})
    end.

%%--------------------------------------------------------------------
%% @spec compile_routes(list(tuple())) -> list()
%% @doc Compiles routes for more quick access
%% @private
%% @end
%%--------------------------------------------------------------------
-spec(compile_routes/1 :: (list(tuple)) -> list()).
compile_routes(Tuples) ->
    Routes = [],
    compile_routes(Tuples, Routes).

%%--------------------------------------------------------------------
%% @spec compile_routes(list(tuple()), list()) -> list()
%% @doc Compiles routes for more quick access
%% @private
%% @end
%%--------------------------------------------------------------------
-spec(compile_routes/2 :: (list(tuple()), list()) -> list()).
compile_routes([], Routes) ->
    Routes;

compile_routes([{root, Params}|Tuples], Routes) ->
    NewRoutes = compile_path(any, [], Params, Routes),
    compile_routes(Tuples, NewRoutes);

compile_routes([{Path, Params} | Tuples], Routes) ->
    compile_routes([{any, Path, Params}| Tuples], Routes);

compile_routes([{Method, Path, Params}|Tuples], Routes) ->
    NewRoutes = compile_path(Method, string:tokens(Path, "/"), Params, Routes),
    compile_routes(Tuples, NewRoutes);

compile_routes([Problem | Tuples], Routes) ->
    io:format("Can't handle route: ~p~n", [Problem]),
    compile_routes(Tuples, Routes).

-spec(compile_path/4 :: (atom(), list(string()), list({atom(), any()}), list()) -> list()).
compile_path(Method, [], Params, Routes) ->
    add_method(Routes, Method, Params);

compile_path(Method, [[$: | Element] | Path], Params, Routes) ->
    NamedParam = case proplists:lookup(named_param, Routes) of
                     none -> [];
                     {_, Result} -> Result
                 end,
    PartList = case proplists:lookup(Element, NamedParam) of
                   none -> [];
                   {_, FoundPart} -> FoundPart
               end,
    UpdatedList = compile_path(Method, Path, Params, PartList),
    UpdatedNamedParam = lists:keystore(Element, 1, NamedParam, {Element, UpdatedList}),
    lists:keystore(named_param, 1, Routes, {named_param, UpdatedNamedParam});
compile_path(Method, [Element| Path], Params, Routes) ->
    PartList = case proplists:lookup(Element, Routes) of
                   none -> [];
                   {_, Result} -> Result
               end,
    UpdatedList = compile_path(Method, Path, Params, PartList),
    lists:keystore(Element, 1, Routes, {Element, UpdatedList}).

-spec(add_method/3 :: (list(), atom(), list({atom(), any()})) -> list()).
add_method(Routes, Method, Params) ->
    case proplists:lookup(root, Routes) of
        none -> [{root, [{Method, Params}]} |  Routes];
        {_, List} -> 
            case lists:keysearch(Method, 1, Routes) of
                false -> [{root, [{Method, Params} | List]}];
                _ -> throw(routing_error)
            end
    end.
%%--------------------------------------------------------------------
%% @spec resolve_route(atom(), string(), #state{}) -> routing_result()
%% @doc Do routing
%% @private
%% @end
%%--------------------------------------------------------------------
-spec(resolve_route/3 :: (atom(), string(), list()) -> routing_result()).
resolve_route(Method, Path, State) ->
    io:format("Path: ~s~n", [Path]),
    io:format("Start dict: ~p~n", [State]),
    SplittedPath = string:tokens(Path, "/"),
    io:format("Splitted Path: ~p~n", [SplittedPath]),
    resolve_route_path(Method, SplittedPath, State).

-spec(resolve_route_path/3 :: (atom(), [nonempty_string()], list()) -> routing_result()).
resolve_route_path(Method, [], Route) ->
    io:format("Current context: ~p~n", [Route]),
    case proplists:lookup(root, Route) of
        {_, Methods} -> check_method(Method, Methods);
        none -> none
    end;
resolve_route_path(Method, [Part| Other], Route) ->
    io:format("Current path: ~s/~p~n", [Part, Other]),
    case proplists:lookup(Part, Route) of
        {_, Value} -> resolve_route_path(Method, Other, Value);
        none -> case proplists:lookup(named_param, Route) of
                    none -> none;
                    {_, Variants} -> resolve_named_path(Method, Variants, Other, Part)
                end
    end.

-spec(resolve_named_path/4 :: (atom(), list(), list(string()), string()) ->
             routing_result()).
resolve_named_path(_Method, [], _PathPart, _Value) ->
    none;
resolve_named_path(Method, [{Name, Route}| Variants], PathPart, Value) ->
    case resolve_route_path(Method, PathPart, Route) of
        {Controller, Action, Params} ->
            NewParams = lists:keystore(Name, 1, Params, {Name, Value}),
            io:format("NewParams: ~p~n", [NewParams]),
            {Controller, Action, NewParams};
        none -> resolve_named_path(Method, Variants, PathPart, Value)
    end.


-spec(check_method/2 :: (atom(), list({atom(), list()})) ->
             routing_result()).
check_method(Method, Methods) ->
    io:format("Searching method ~p in ~p~n", [Method, Methods]),
    case lists:keysearch(Method, 1, Methods) of
        {_, {_, Params}} -> to_route_result(Params);
        false -> case lists:keysearch(any, 1, Methods) of
                     {_, {_, Params}} -> to_route_result(Params);
                     false -> none
                 end
    end.

%%--------------------------------------------------------------------
%% @spec to_route_result(list()) -> routing_result()
%% @doc Convertes route params to result action
%% @private
%% @end
%%--------------------------------------------------------------------
-spec(to_route_result/1 :: (list({any(), any()})) -> routing_result()).
to_route_result(Params)->
    io:format("Routed params: ~p~n", [Params]),
    Controller = proplists:get_value(controller, Params, ""),
    Action = proplists:get_value(action, Params, "index"),
    {Controller, Action, cleanup_parameters(Params)}.


%%--------------------------------------------------------------------
%% @spec cleanup_parameters(list({atom, any()})) -> list({atom, any()})
%% @doc Removes controller/action parameters from parameter list
%% @private
%% @end
%%--------------------------------------------------------------------
-spec(cleanup_parameters/1 :: (list({atom(), any()})) -> list({atom(), any()})).
cleanup_parameters(Params) ->
    cleanup_parameters(Params, []).

%%--------------------------------------------------------------------
%% @spec cleanup_parameters(list({atom, any()}), list()) -> list({atom, any()})
%% @doc Removes controller/action parameters from parameter list
%% @private
%% @end
%%--------------------------------------------------------------------
-spec(cleanup_parameters/2 :: (list({atom, any()}), list({atom, any()})) -> list({atom, any()})).
cleanup_parameters([], Acc) ->
    Acc;
cleanup_parameters([{controller, _} | Params], Acc) ->
    cleanup_parameters(Params, Acc);
cleanup_parameters([{action, _} | Params], Acc) ->
    cleanup_parameters(Params, Acc);
cleanup_parameters([Head | Params], Acc) ->
    cleanup_parameters(Params, [Head | Acc]).

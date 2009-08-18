%%%-------------------------------------------------------------------
%%% @author Alexander Borovsky <partizan@altlinux.ru>
%%% @doc Request dispatcher module
%%% @version 0.0.1
%%% @end
%%%-------------------------------------------------------------------
-module(s_dispatcher).

%% API
-export([dispatch/1]).
-include("s_internal_types.hrl").

-define(Million, 1000000).

%%====================================================================
%% API
%%====================================================================

%%
%% @spec dispatch(#common_request_record{}) -> steroids_response()
%% @doc Dispatches request to target controller.
%% @end
%%
-spec(dispatch/1 :: (#common_request_record{}) ->
             steroids_response()).
dispatch(Request) ->
    StartTime = erlang:now(),
    try do_dispatch(Request) of
        Result -> Result
    catch
        Type:Error -> process_error({error, Type, Error})
    after
        s_log:log(info, ?MODULE, "Request processed in ~f", 
                  [timer:now_diff(erlang:now(), StartTime) / ?Million])
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%%
%% @spec merge_parameters(list(request_parameters_list())) -> gb_tree()
%% @doc Converts parameter list to map like structure
%% @private
%% @end
%% 
-spec(merge_parameters/1 :: (list(request_parameters_list())) ->
             gb_tree()).
merge_parameters(ParameterLists) ->
    Params = lists:flatten(ParameterLists),
    to_map(Params).

%%
%% @spec to_map(request_parameters_list()) -> gb_tree()
%% @doc Converts parameter list to map like structure
%% @private
%% @end
%% 
-spec(to_map(request_parameters_list()) -> gb_tree()).
to_map(List) ->
    gb_trees:balance(to_map(List, gb_trees:empty())).

-spec(to_map(request_parameters_list(), gb_tree()) -> gb_tree()).
to_map([], Tree) ->
    Tree;
to_map([{Key, Value} | Tail], Tree) ->
    case gb_trees:is_defined(Key, Tree) of
        true ->
            to_map(Tail, Tree);
        false -> to_map(Tail, gb_trees:insert(Key, Value, Tree))
    end.

%%
%% @spec process_request(#dispatched_request_record{}) -> steroids_response()
%% @doc Executes requested controller and renders view
%% @private
%% @end
%% 
-spec(process_request/1 :: (#dispatched_request_record{}) ->
             steroids_response()).
process_request(#dispatched_request_record{
                 controller = Controller,
                 action = ActionName,
                 parameters = Parameters} = Request) ->
    s_context:init(Request),
    ControllerModule = s_reloader:load_thing(s_controller_loader, Controller),
    Action = list_to_existing_atom(ActionName),
    Result = apply(ControllerModule, Action, [Parameters]),
    process_controller_result(Request, Result).

-spec(process_controller_result/2 :: (#dispatched_request_record{}, 
                                      steroids_controller_result()) ->
             steroids_response()).
process_controller_result(#dispatched_request_record{
                           controller = Controller,
                           action = Action
                          } = Request,  ok) ->
    process_controller_result(Request, {render, Controller, Action});
process_controller_result(_,  {redirect, Target}) ->
    #redirect_response{target = Target};

process_controller_result(Request, {render, Controller, Action}) when 
  is_atom(Controller) ->
    process_controller_result(Request, {render, atom_to_list(Controller), Action});

process_controller_result(Request, {render, Controller, Action}) when 
  is_atom(Action) ->
    process_controller_result(Request, {render, Controller, atom_to_list(Action)});

process_controller_result(Request, {render, Controller, Action}) ->
    Path = Controller ++ "/" ++ Action,
    process_controller_result(Request, {render, Path});

process_controller_result(_Request, {render, Path}) ->
    Data = s_templates:render(Path, dict:new()),
    #render_response{data=Data, content_type = "text/html", status_code=200}.


-spec(do_dispatch/1 :: (#common_request_record{}) ->
             steroids_response()).
do_dispatch(#common_request_record{method = Method, 
                                url = URL, 
                                get_params = GetParams, 
                                post_params = PostParams
                               }) ->
    case s_routes:parse_request(Method, URL) of
        {Controller, Action, RequestParameters} -> 
            Parameters = merge_parameters([RequestParameters, GetParams, PostParams]),
            DispatchedRequest = #dispatched_request_record {
              method = Method,
              controller = Controller,
              action = Action,
              parameters = Parameters
             },
            s_log:log(trace, ?MODULE, "Routing request ~p~n", [DispatchedRequest]),
            process_request(DispatchedRequest);
        not_found -> 
            s_log:log(error, ?MODULE, "Error routing ~p request to ~s~n", [Method, URL]),
            process_error(error_404)
    end.

process_error(error_404) ->
    #render_response{data="<html><head></head><body><h1>Page not found</h1></body></body>", content_type = "text/html", status_code=404};

process_error({error, Type, Reason}) ->
    BackTrace = erlang:get_stacktrace(),
    s_log:log(error, s_dispatcher, "Internal error ~p:~p~n~p", [Type, Reason, BackTrace]),
    #render_response{data="<html><head></head><body><h1>Internal error</h1></body></body>", content_type = "text/html", status_code=500}.

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
dispatch(#common_request_record{method = Method, 
                                url = URL, 
                                get_params = GetParams, 
                                post_params = PostParams
                               }) ->
    {Controller, Action, RequestParameters} = s_routes:parse_request(Method, URL),
    Parameters = merge_parameters([RequestParameters, GetParams, PostParams]),
    DispatchedRequest = #dispatched_request_record {
      method = Method,
      controller = Controller,
      action = Action,
      parameters = Parameters
     },
    process_request(DispatchedRequest).

%%====================================================================
%% Internal functions
%%====================================================================

%%
%% @spec merge_parameters(list(request_parameters_list())) -> dict()
%% @doc Converts parameter list to dict
%% @private
%% @end
%% 
-spec(merge_parameters/1 :: (list(request_parameters_list())) ->
             dict()).
merge_parameters(ParameterLists) ->
    Params = lists:flatten(ParameterLists),
    dict:from_list(Params).

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


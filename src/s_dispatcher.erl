%%%-------------------------------------------------------------------
%%% File    : s_dispatcher.erl
%%% Author  : Alexander Borovsky <partizan@altlinux.ru>
%%% Description : Request dispatcher module
%%%
%%% Created : 12 Jul 2009 by Alexander Borovsky <partizan@altlinux.ru>
%%%-------------------------------------------------------------------
-module(s_dispatcher).

%% API
-export([dispatch/1]).
-include("s_internal_types.hrl").

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: Dispatches request
%% Description: Dispatches request to target controller.
%%--------------------------------------------------------------------
-spec(dispatch/1 :: (#common_request_record{}) ->
             steroids_response()).
dispatch(#common_request_record{method = Method, 
                                url = URL, 
                                get_params = GetParams, 
                                post_params = PostParams
                               }) ->
    {Controller, Action, RequestParameters} = s_routes:parse_request(Method, URL),
    Parameters = merge_parameters([RequestParameters, GetParams, PostParams]),
    process_request(Controller, Action, Parameters).

%% TODO: Add 
%% Converts parameter list to dict
-spec(merge_parameters/1 :: (list(list(request_parameter()))) ->
             tuple()).
merge_parameters(ParameterLists) ->
    Params = list:flatten(ParameterLists),
    dict:from_list(Params).

process_request(Controller, Action, Parameters) ->
    Data = "<html><head><title>Test</title></head><body><h1>Test</h1></body></html>",
    #render_response{data=Data, content_type = "text/html", status_code=200}.

%%====================================================================
%% Internal functions
%%====================================================================

%%% @author Alexander Borovsky <partizan@altlinux.ru>
%%% @doc Module for access to request-time data (parameters, session, etc)
%%% @version 0.0.1
%%% @private
-module(s_context).

%% API
-export([get/1, put/2,
         get_param/1, 
         get_context/0, 
         get_controller/0,
         get_action/0,
         get_method/0,
         init/1, cleanup/0]).


-include("s_internal_types.hrl").

%%====================================================================
%% API
%%====================================================================

%%
%% @spec init(#dispatched_request_record{}) -> any()
%% @doc Populates request context
%% @end
%% 
-spec(init/1 :: (#dispatched_request_record{}) -> any()).
init(#dispatched_request_record{
        action = Action,
        controller = Controller,
        method = Method,
        parameters = Params
      }) ->
    erlang:put('__param', Params),
    erlang:put('__action', Action),
    erlang:put('__controller', Controller),
    erlang:put('__method', Method).


%%
%% @spec cleanup() -> any()
%% @doc Cleanups request context
%% @end
%% 
-spec(cleanup/0 :: () ->
             any()).
cleanup() ->
    erlang:erase().

%%
%% @spec get_param(atom()) -> any()
%% @doc Returns request parameter
%% @end
%% 
-spec(get_param/1 :: (atom()) -> any()).
get_param(Key) ->
    Params = erlang:get('__param'),
    case gb_trees:lookup(Key, Params) of
        none -> no_found;
        {value, Value} -> Value
    end.

%%
%% @spec get_controller() -> string()
%% @doc Returns controller, that processes request
%% @end
%% 
-spec(get_controller/0 :: () -> string()).
get_controller() ->
    erlang:get('__controller').

%%
%% @spec get_action() -> string()
%% @doc Returns action, that processes request
%% @end
%% 
-spec(get_action/0 :: () -> string()).
get_action() ->
    erlang:get('__action').

%%
%% @spec get_method() -> string()
%% @doc Returns request method
%% @end
%% 
-spec(get_method/0 :: () -> atom()).
get_method() ->
    erlang:get('__method').

%%
%% @doc Returns variable from request's context
%% @end
%% 
-spec(get/1 :: (atom()) -> any()).
get(Key) ->
    erlang:get(Key).

%%
%% @doc Puts variable to request's context
%% @end
%% 
-spec(put/2 :: (atom(), any()) -> any()).
put(Key, Value) ->
    erlang:put(Key, Value).

-spec(get_context() -> any()).
get_context() ->
    erlang:get().
%%====================================================================
%% Internal functions
%%====================================================================

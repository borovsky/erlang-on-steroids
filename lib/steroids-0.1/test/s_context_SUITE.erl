%%%-------------------------------------------------------------------
%%% File    : s_context_SUITE.erl
%%% Author  : Alexander Borovsky <partizan@altlinux.ru>
%%% Description : 
%%%
%%% Created : 23 Jul 2009 by Alexander Borovsky <partizan@altlinux.ru>
%%%-------------------------------------------------------------------
-module(s_context_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ct.hrl").
-include("s_internal_types.hrl").

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    s_context:init(create_request()),
    Config.

create_request() ->
    Params = gb_trees:from_orddict([{"param1", "test"}, {"param2", "test3"}]),
    #dispatched_request_record{
      method = get,
      controller = "my_test",
      action = "index",
      parameters = Params
     }.

end_per_testcase(_TestCase, _Config) ->
    s_context:cleanup(),
    ok.

all() -> 
    [check_params, check_controller, check_action, check_method].


check_params(_Config) -> 
    "test" = s_context:get_param("param1"),
    "test3" = s_context:get_param("param2").

check_controller(_Config) -> 
    "my_test" = s_context:get_controller().

check_action(_Config) ->
    "index" = s_context:get_action().

check_method(_Config) ->
    get = s_context:get_method().


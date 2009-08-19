%%%-------------------------------------------------------------------
%%% @author Alexander Borovsky <partizan@altlinux.ru>
%%% @doc Test for templates system
%%%-------------------------------------------------------------------
-module(s_template_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ct.hrl").

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    s_context:cleanup(),
    ok.

all() -> 
    [test_stored_set, test_stored_append].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

test_stored_set(_Config) -> 
    s_template:set_stored(my, "Test"),
    <<"Test">> = list_to_binary(s_template:get_stored(my)).

test_stored_append(_Config) -> 
    s_template:set_stored(my, "Test"),
    s_template:append_stored(my, "XYZ"),
    s_template:append_stored(my, "ABC"),
    <<"TestXYZABC">> = list_to_binary(s_template:get_stored(my)).

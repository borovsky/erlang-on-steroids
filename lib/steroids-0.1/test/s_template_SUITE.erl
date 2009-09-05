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
    [test_block_set, test_block_append, test_have_block].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

test_block_set(_Config) -> 
    s_template:set_block(my, "Test"),
    <<"Test">> = list_to_binary(s_template:get_block(my)).

test_block_append(_Config) -> 
    s_template:set_block(my, "Test"),
    s_template:append_block(my, "XYZ"),
    s_template:append_block(my, "ABC"),
    <<"TestXYZABC">> = list_to_binary(s_template:get_block(my)).

test_have_block(_Config) ->
    false = s_template:have_block(my),
    s_template:set_block(my, "Test"),
    true = s_template:have_block(my).


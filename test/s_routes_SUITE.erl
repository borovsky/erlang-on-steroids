%%%-------------------------------------------------------------------
%%% File    : s_reloader_SUITE.erl
%%% Author  : Alexander Borovsky <partizan@altlinux.ru>
%%% Description : 
%%%
%%% Created :  2 Aug 2009 by Alexander Borovsky <partizan@altlinux.ru>
%%%-------------------------------------------------------------------
-module(s_routes_SUITE).

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
    s_conf:start(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    s_conf:terminate(),
    ok.

all() -> 
    [test_root_route, test_incomplete_paramters,
    test_static_page_path, test_static_page_hierarchy,
    test_substitution_page_hierarchy, 
    test_rest_route].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

calc_routes(Method, Path, Routes) ->
    CompiledRoutes = s_routes:compile_routes(Routes),
    s_routes:resolve_route(Method, Path, CompiledRoutes).
    

test_root_route(_Config) -> 
    {"main", "index", []} = 
        calc_routes(get, "", 
                    [{root, [{controller, "main"}, {action, "index"}]}]),
    ok.

test_incomplete_paramters(_Config) -> 
    {"main", "index", []} =         
        calc_routes(get, "", 
                    [{root, [{controller, "main"}]}]),

    ok.

test_static_page_path(_Config) -> 
    {"test", "index", []} = 
        calc_routes(get, "test", 
                    [{"test", [{controller, "test"}]}]),
    ok.

test_static_page_hierarchy(_Config) -> 
    {"test2", "index", []} = 
        calc_routes(get, "test/test2", 
                    [{"test", [{controller, "test"}]},
                     {"test/test2", [{controller, "test2"}]}]),
    ok.

test_substitution_page_hierarchy(_Config) -> 
    {"test2", "index", [{"id", "123"}]} = 
        calc_routes(get, "test/123", 
                    [{"test/:id", [{controller, "test2"}]}]),
    ok.

test_rest_route(_Config) -> 
    {"test", "index", []} = 
        calc_routes(get, "test", 
                    [{get, "test", [{controller, "test"}]},
                     {post, "test", [{controller, "test2"}]}]),
    {"test2", "index", []} = 
        calc_routes(post, "test", 
                    [{get, "test", [{controller, "test"}]},
                     {post, "test", [{controller, "test2"}]}]),
    ok.

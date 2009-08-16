%%%-------------------------------------------------------------------
%%% File    : s_reloader_SUITE.erl
%%% Author  : Alexander Borovsky <partizan@altlinux.ru>
%%% Description : 
%%%
%%% Created :  2 Aug 2009 by Alexander Borovsky <partizan@altlinux.ru>
%%%-------------------------------------------------------------------
-module(s_reloader_SUITE).

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
    s_reloader:start(),
    
    PrivDir = proplists:get_value(priv_dir, Config),
    CompileFilePath = filename:join(PrivDir, "test_compile.tst"),
    file:write_file(CompileFilePath, "test"),
    
    CompileAndLoad = 
        fun(RealPath, Module) -> 
                RealPath = CompileFilePath,
                Module = test_loader,
                ExecutionVal = s_conf:get(compile_and_load_executions),
                s_conf:set(compile_and_load_executions, [$+ | ExecutionVal])
        end,
    GetRealPath = 
        fun(Path) -> Path = "my_test_path",
                     ExecutionVal = s_conf:get(get_real_path_executions),
                     s_conf:set(get_real_path_executions, [$+ | ExecutionVal]),
                     CompileFilePath                               
        end,
    GetModuleName = 
        fun(Path) -> Path = "my_test_path",
                     ExecutionVal = s_conf:get(get_module_name_executions),
                     s_conf:set(get_module_name_executions, [$+ | ExecutionVal]),
                     "test_loader"
        end,
    s_conf:set(test_compile_and_load, CompileAndLoad),
    s_conf:set(test_get_real_path, GetRealPath),
    s_conf:set(test_get_module_name, GetModuleName),
    s_conf:set(compile_and_load_executions, ""),
    s_conf:set(get_real_path_executions, ""),
    s_conf:set(get_module_name_executions, ""),

    [{test_file_name, CompileFilePath}] ++ Config.

end_per_testcase(_TestCase, _Config) ->
    s_conf:terminate(),
    s_reloader:terminate(),
    ok.

all() -> 
    [test_compile, test_reload_cache, test_recompile_if_changed].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

test_compile(_Config) -> 
    test_loader = s_reloader:load_thing(test_loader, "my_test_path"),
    "+" = s_conf:get(compile_and_load_executions),
    "+" = s_conf:get(get_real_path_executions),
    "++" = s_conf:get(get_module_name_executions),
    ok.

test_reload_cache(_Config) -> 
    test_loader = s_reloader:load_thing(test_loader, "my_test_path"),
    test_loader = s_reloader:load_thing(test_loader, "my_test_path"),

    "+" = s_conf:get(compile_and_load_executions),
    "+" = s_conf:get(get_real_path_executions),
    "+++" = s_conf:get(get_module_name_executions),
    ok.

test_check_after_timeout(_Config) -> 
    test_loader = s_reloader:load_thing(test_loader, "my_test_path"),
    s_reloader:timeout_module("test_loader"),
    test_loader = s_reloader:load_thing(test_loader, "my_test_path"),

    "+" = s_conf:get(compile_and_load_executions),
    "++" = s_conf:get(get_real_path_executions),
    "+++" = s_conf:get(get_module_name_executions),
    ok.

test_recompile_if_changed(Config) -> 
    test_loader = s_reloader:load_thing(test_loader, "my_test_path"),
    CompileFilePath = proplists:get_value(test_file_name, Config),
    Sec = calendar:datetime_to_gregorian_seconds(erlang:localtime()),
    ok = file:change_time(CompileFilePath, calendar:gregorian_seconds_to_datetime(Sec+1)),

    s_reloader:timeout_module("test_loader"),
    test_loader = s_reloader:load_thing(test_loader, "my_test_path"),

    "++" = s_conf:get(compile_and_load_executions),
    "++++" = s_conf:get(get_real_path_executions),
    "++++" = s_conf:get(get_module_name_executions),
    ok.

%%%-------------------------------------------------------------------
%%% File    : s_conf_SUITE.erl
%%% Author  : Alexander Borovsky <partizan@altlinux.ru>
%%% Description : 
%%%
%%% Created : 11 Jul 2009 by Alexander Borovsky <partizan@altlinux.ru>
%%%-------------------------------------------------------------------
-module(s_log_SUITE).

-compile(export_all).

-include("ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    s_conf:start(),
    LogFile = filename:join(PrivDir, "file.log"),
    s_conf:set(log_file, LogFile),
    s_conf:set(log_level, info),
    file:delete(LogFile),
    s_log:start(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    s_log:terminate(),
    s_conf:terminate(),
    ok.

all() -> 
    [test_log_levels].


test_log_levels(_Config) -> 
    LogFile = s_conf:get(log_file),
    Empty = 0,
    Empty = filelib:file_size(LogFile),

    s_log:log(trace, test_module, "Test Message", []),
    s_log:flush(),
    Empty = filelib:file_size(LogFile),

    s_log:log(debug, test_module, "Test Message", []),
    s_log:flush(),
    Empty = filelib:file_size(LogFile),
    
    s_log:log(info, test_module, "Test Message", []),
    s_log:flush(),
    AfterInfoSize = filelib:file_size(LogFile),
    true = AfterInfoSize > 0,

    s_log:log(warn, test_module, "Test Message", []),
    s_log:flush(),
    AfterWarnSize = filelib:file_size(LogFile),
    true = AfterWarnSize > AfterInfoSize,

    s_log:log(error, test_module, "Test Message", []),
    s_log:flush(),
    AfterErrorSize = filelib:file_size(LogFile),
    true =  AfterErrorSize > AfterWarnSize,

    s_log:log(fatal, test_module, "Test Message", []),
    s_log:flush(),
    AfterFatalSize = filelib:file_size(LogFile),
    true = AfterFatalSize > AfterErrorSize,

    ok.

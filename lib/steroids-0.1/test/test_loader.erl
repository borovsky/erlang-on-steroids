%%%-------------------------------------------------------------------
%%% File    : test_loader.erl
%%% Author  : Alexander Borovsky <partizan@altlinux.ru>
%%% Description : Test loader
%%%
%%% Created :  2 Aug 2009 by Alexander Borovsky <partizan@altlinux.ru>
%%%-------------------------------------------------------------------
-module(test_loader).

-behaviour(s_reloader).

-export([compile_and_load/2, get_real_path/1, get_module_name/1]).

%%====================================================================
%% API
%%====================================================================
-spec(compile_and_load/2 :: (string(), atom()) -> any()).
compile_and_load(RealPath, Module) ->
    Func = s_conf:get(test_compile_and_load),
    Func(RealPath, Module).

-spec(get_real_path/1 :: (string()) -> string()).
get_real_path(Path) ->
    Func = s_conf:get(test_get_real_path),
    Func(Path).

-spec(get_module_name/1 :: (string()) -> string()).
get_module_name(Path) ->
    Func = s_conf:get(test_get_module_name),
    Func(Path).

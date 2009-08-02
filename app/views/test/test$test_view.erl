%%%-------------------------------------------------------------------
%%% File    : test_view.erl
%%% Author  : Alexander Borovsky <partizan@altlinux.ru>
%%% Description : 
%%%
%%% Created : 16 Jul 2009 by Alexander Borovsky <partizan@altlinux.ru>
%%%-------------------------------------------------------------------
-module('test$test_view').

%% API
-export([render/1]).

render(_Params) ->
    "<html><head><title>Test</title></head><body><h1>Test2</h1></body></html>".


%%%-------------------------------------------------------------------
%%% File    : s_erlydtl_adapter.erl
%%% Author  : Alexander Borovsky <partizan@altlinux.ru>
%%% Description : 
%%%
%%% Created : 19 Aug 2009 by Alexander Borovsky <partizan@altlinux.ru>
%%%-------------------------------------------------------------------
-module(s_erlydtl_adapter).

%% API
-behaviour(s_template).
-export([compile/2, extensions/0]).

%%====================================================================
%% API
%%====================================================================

%%
%% @spec extensions() -> list(string())
%% @doc Returns list of extensions for ErlyDTL templating engine
%% @end
%%
-spec(extensions() -> list(string())).
extensions() ->              
    ["dtl"].

%%
%% @spec compile(string(), atom()) -> ok | {error, any()}
%% @doc Compiles ErlyDTL template to Module
%% @end
%%
-spec(compile(string(), atom()) -> ok | {error, any()}).
compile(Path, Module) ->
    erlydtl:compile(Path, Module, [{out_dir, s_conf:get(ebin_dir)}]).

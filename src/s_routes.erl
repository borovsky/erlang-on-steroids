%%%-------------------------------------------------------------------
%%% File    : s_routes.erl
%%% Author  : Alexander Borovsky <partizan@altlinux.ru>
%%% Description : 
%%%
%%% Created : 12 Jul 2009 by Alexander Borovsky <partizan@altlinux.ru>
%%%-------------------------------------------------------------------
-module(s_routes).

%% API
-export([parse_request/2]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
parse_request(_Method, _URL) ->
    {"test", "test", []}.


%%====================================================================
%% Internal functions
%%====================================================================

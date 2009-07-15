%%%-------------------------------------------------------------------
%%% File    : my_start.erl
%%% Author  : Alexander Borovsky <alex@partizan.home>
%%% Description : 
%%%
%%% Created : 24 Jun 2009 by Alexander Borovsky <alex@partizan.home>
%%%-------------------------------------------------------------------
-module(s_start).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start/0, start/1
        ]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------


%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------

start() ->
    start(inets).

start(Server) ->
    io:format("Loading application...~n", []),
    application:load(steroids),
    io:format("Starting application...~n", []),
    application:start(steroids),
    io:format("Starting webserver...~n", []),
    start_webserver(Server).

start_webserver(inets) ->
    inets:stop(),
    application:set_env(inets, services, [{httpd, filename:join([s_conf:server_root(), "config", "inets.conf"])}]),

    inets:start().

%%====================================================================
%% Internal functions
%%====================================================================

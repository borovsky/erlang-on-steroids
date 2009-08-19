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
-spec(start() -> any()).             
start() ->
    start(inets).

-spec(start(atom()) -> any()).             
start(Server) ->
    appmon:start(),
    io:format("Starting application...~n", []),
    application:start(steroids),
    s_template_loader:init(),
    io:format("Starting webserver...~n", []),
    start_webserver(Server).


%%====================================================================
%% Internal functions
%%====================================================================
-spec(start_webserver(atom()) -> any()).             
start_webserver(inets) ->
    inets:stop(),
    application:set_env(inets, services, [{httpd, filename:join([s_conf:server_root(), "config", "inets.conf"])}]),

    inets:start().

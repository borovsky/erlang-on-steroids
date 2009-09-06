%%%
%%% This Library is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU Library General Public License as
%%% published by the Free Software Foundation; either version 3 of the
%%% License, or (at your option) any later version.
%%%
%%% This Library is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% Library General Public License for more details.
%%%
%%% You should have received a copy of the GNU Library General Public
%%% License along with the Gnome Library; see the file COPYING.LIB.  If not,
%%% write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
%%% Boston, MA 02111-1307, USA.
%%%

%%%
%%% @author Alexander Borovsky <alex@partizan.home>
%%% @doc Steroids starter
%%% 

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

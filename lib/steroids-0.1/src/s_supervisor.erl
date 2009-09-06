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
%%% @author Alexander Borovsky <partizan@altlinux.ru>
%%% @doc Supervisor for steroids framework
%%%
-module(s_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).


%%====================================================================
%% API functions
%%====================================================================

%%
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the supervisor
%%
-spec(start_link/1 :: (any()) -> {ok, pid()} | ignore | {error, any()}).
start_link(_StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%%
%% @spec init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% @doc Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%
-spec(init/1 :: (any()) -> {ok, tuple()} | ignore | {error, any()}).
init([]) ->
    SConf = {s_conf,{s_conf,start_link,[]},
              permanent,2000,worker,[s_conf]},
    SLog = {s_log,{s_log,start_link,[]},
              permanent,2000,worker,[s_conf]},
    SReloader = {s_reloader,{s_reloader,start_link,[]},
              permanent,2000,worker,[s_conf]},
    Routes = generate_routes(),
    {ok,{{one_for_all,0,1}, [SConf, SLog, SReloader] ++ Routes}}.

%%====================================================================
%% Internal functions
%%====================================================================

generate_routes() ->
    Indexes = lists:seq(1, erlang:system_info(schedulers)),
    
    lists:map(fun(Idx) -> 
                      Id = list_to_atom("s_routes" ++ integer_to_list(Idx)),
                      {Id,{s_routes,start_link,[]},
                       permanent,2000,worker,[s_routes]} end, Indexes).

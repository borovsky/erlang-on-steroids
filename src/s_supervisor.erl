%%%-------------------------------------------------------------------
%%% File    : s_supervisor.erl
%%% Author  : Alexander Borovsky <partizan@altlinux.ru>
%%% Description : 
%%%
%%% Created : 12 Jul 2009 by Alexander Borovsky <partizan@altlinux.ru>
%%%-------------------------------------------------------------------
-module(s_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).


%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(_StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
-spec(init/1 :: (any()) -> {ok, tuple()} | ignore | {error, any()}).
init([]) ->
    SConf = {s_conf,{s_conf,start_link,[]},
              permanent,2000,worker,[s_conf]},
    SReloader = {s_reloader,{s_reloader,start_link,[]},
              permanent,2000,worker,[s_conf]},
    {ok,{{one_for_all,0,1}, [SConf, SReloader]}}.

%%====================================================================
%% Internal functions
%%====================================================================

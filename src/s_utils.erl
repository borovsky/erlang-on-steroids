%%%-------------------------------------------------------------------
%%% File    : s_utils.erl
%%% Author  : Alexander Borovsky <partizan@altlinux.ru>
%%% Description : Some utilites
%%%
%%% Created : 12 Jul 2009 by Alexander Borovsky <partizan@altlinux.ru>
%%%-------------------------------------------------------------------
-module(s_utils).

%% API
-export([mkdir_p/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: mkdir_p/1
%% Description: Creates dir and it's parent
%%--------------------------------------------------------------------
mkdir_p(DirName) ->
    case file:read_file_info(DirName) of
        {ok, _} -> ok;
        {error, _} -> 
            case mkdir_p(filename:dirname(DirName)) of
                ok -> file:make_dir(DirName);
                {error, _} = Error -> Error
            end
        end.


%%====================================================================
%% Internal functions
%%====================================================================

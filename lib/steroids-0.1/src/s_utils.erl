%%%-------------------------------------------------------------------
%%% @author Alexander Borovsky <partizan@altlinux.ru>
%%% @doc Some utilites
%%% @end
%%%-------------------------------------------------------------------
-module(s_utils).

%% API
-export([mkdir_p/1, iso_8601_fmt/1]).

-include_lib("kernel/include/file.hrl").

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @spec mkdir_p(string()) -> ok | {error, any()}
%% @doc Creates dir and it's parent
%% @end
%%--------------------------------------------------------------------
-spec(mkdir_p/1 :: (string()) -> ok | {error, any()}).
mkdir_p(DirName) ->
    case file:read_file_info(DirName) of
        {ok, _} -> ok;
        {error, _} -> 
            case mkdir_p(filename:dirname(DirName)) of
                ok -> file:make_dir(DirName);
                {error, _} = Error -> Error
            end
        end.

%%--------------------------------------------------------------------
%% @spec iso_8601_fmt(date_time()) -> string()
%% @doc Formates dates in ISO-8601 format
%% @end
%%--------------------------------------------------------------------
-spec(iso_8601_fmt/1 :: (date_time()) -> string()).
iso_8601_fmt(DateTime) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
        [Year, Month, Day, Hour, Min, Sec]).

%%====================================================================
%% Internal functions
%%====================================================================

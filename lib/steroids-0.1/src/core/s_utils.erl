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
%%% @doc Some utility functions
%%%
-module(s_utils).

%% API
-export([iso_8601_fmt/1]).

-include_lib("kernel/include/file.hrl").

%%====================================================================
%% API
%%====================================================================

%%
%% @spec iso_8601_fmt(date_time()) -> string()
%% @doc Formates dates in ISO-8601 format
%% 
-spec(iso_8601_fmt/1 :: (date_time()) -> string()).
iso_8601_fmt(DateTime) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
        [Year, Month, Day, Hour, Min, Sec]).

%%====================================================================
%% Internal functions
%%====================================================================

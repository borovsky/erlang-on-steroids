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
%%% @author Alexander Borovsky <alex.borovsky@gmail.com>
%%% @doc Loader for templates
%%% @end
%%%
-module(s_controller_loader).

-behaviour(s_reloader).

-export([compile_and_load/2, get_real_path/1, get_module_name/1]).

-spec(compile_and_load/2 :: (string(), atom()) -> any()).
compile_and_load(RealPath, _Module) ->
    Path = filename:rootname(RealPath),
    compile:file(Path, [bin_opt_info, debug_info, report,
                       {outdir, s_conf:get(ebin_dir)}]),
    ok.

%%
%% @spec get_real_path(string()) -> string()
%% @doc Returns the path to site root dir
%% @private
%% @end
%%
-spec(get_real_path/1 :: (string()) -> string()).
get_real_path(Path) ->
    filename:join(s_conf:get(controller_dir), Path ++ "_controller.erl").

%% TODO: We doesn't support controllers in subdirs yet
-spec(get_module_name/1 :: (string()) -> string()).
get_module_name(Path) ->
    Path ++ "_controller".

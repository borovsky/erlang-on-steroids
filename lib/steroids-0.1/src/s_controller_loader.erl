%%%-------------------------------------------------------------------
%%% @author Alexander Borovsky <partizan@altlinux.ru>
%%% @doc Loader for templates
%%% @end
%%%-------------------------------------------------------------------
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

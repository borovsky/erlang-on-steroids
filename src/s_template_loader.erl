%%%-------------------------------------------------------------------
%%% @author Alexander Borovsky <partizan@altlinux.ru>
%%% @doc Loader for templates
%%% @end
%%%-------------------------------------------------------------------
-module(s_template_loader).

-behaviour(s_reloader).

-export([compile_and_load/2, get_real_path/1, get_module_name/1]).

-define(MODULE_SUFFIX, "_view").

-spec(compile_and_load/2 :: (string(), atom()) -> any()).
compile_and_load(RealPath, Module) ->
    TargetDir = s_conf:get(ebin_dir),
    FileName = atom_to_list(Module),
    TargetFileBase = filename:join(TargetDir, FileName),
    TargetFile = TargetFileBase ++ ".erl",
    file:copy(RealPath, TargetFile),
    compile:file(TargetFile, [bin_opt_info, debug_info, report,
                       {outdir, s_conf:get(ebin_dir)}]),
    file:delete(TargetFile),
    ok.

%%
%% @spec get_real_path(string()) -> string()
%% @doc Returns the path to site root dir
%% @private
%% @end
%%
-spec(get_real_path/1 :: (string()) -> string()).
get_real_path(Path) ->
    filename:join([s_conf:get(views_dir), Path ++ ".erl"]).

-spec(get_module_name/1 :: (string()) -> string()).
get_module_name(Path) ->
    Tokens = string:tokens(Path, "/"),
    string:join(Tokens, "$") ++ ?MODULE_SUFFIX.

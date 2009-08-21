%%%-------------------------------------------------------------------
%%% @author Alexander Borovsky <partizan@altlinux.ru>
%%% @doc Loader for templates
%%% @end
%%%-------------------------------------------------------------------
-module(s_template_loader).

-behaviour(s_reloader).

-export([init/0]).
-export([compile_and_load/2, get_real_path/1, get_module_name/1]).

%% Behaviour
-export([behaviour_info/1]).

-define(MODULE_SUFFIX, "_view").

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), byte()}].
behaviour_info(callbacks) ->
    [{compile, 2}];
behaviour_info(_Other) ->
    undefined.

%%
%% @spec init() -> ok
%% @doc Initializes templating system
%% @private
%% @end
%%
-spec(init() -> ok).
init() ->
    TemplateEngines = s_conf:get(template_engines),
    compile_engines_definitions(TemplateEngines).

%%
%% @spec compile_and_load(string(), atom()) -> any()
%% @doc Compiles and loads resource
%% @private
%% @end
%%
-spec(compile_and_load(string(), atom()) -> ok | compile_error).
compile_and_load(RealPath, TargetModule) ->
    Ext = lists:nthtail(1, filename:extension(RealPath)),
    ExtensionsMap = s_conf:get('__templates_extension_map'),
    io:format("Ext: ~p", [Ext]),
    io:format("ExtMap: ~p", [ExtensionsMap]),
 
    case gb_trees:lookup(Ext, ExtensionsMap) of
        none ->
            s_log:fatal(?MODULE, "Can't detect compile module for '~s'. Internal error?", [RealPath]),
            internal_error;
        {value, Value} -> 
            CompileModule = Value,
            case apply(CompileModule, compile, [RealPath, TargetModule]) of
                ok -> ok;
                {error, Reason} -> 
                    s_log:error(?MODULE, "Compile error: ~s", [Reason]),
                    compile_error
            end
    end.

%%
%% @spec get_real_path(string()) -> string()
%% @doc Returns the path to site root dir
%% @private
%% @end
%%
-spec(get_real_path/1 :: (string()) -> string()).
get_real_path(Path) ->
    PathBegin = filename:join(s_conf:get(views_dir), Path ++ "."),
    Extensions = s_conf:get('__templates_supported_extensions'),
    
    case lists:dropwhile(fun(Ext) -> not(filelib:is_regular(PathBegin ++ Ext)) end, Extensions) of
        [] -> not_found;
        [Hd | _] -> 
                       PathBegin ++ Hd
    end.

%%
%% @spec get_module_name(string()) -> string()
%% @doc Returns module name for specified virtual path
%% @private
%% @end
%%
-spec(get_module_name/1 :: (string()) -> string()).
get_module_name(Path) ->
    Tokens = string:tokens(Path, "/"),
    string:join(Tokens, "$") ++ ?MODULE_SUFFIX.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


-spec(compile_engines_definitions(list(tuple())) -> ok).
compile_engines_definitions(Engines) ->
    SupportedExtensions = 
        lists:append(lists:map(fun({_Module, Extensions}) -> Extensions end, Engines)),
    ExtensionMap = 
        lists:foldl(fun append_extensions/2, gb_trees:empty(), Engines),
    s_conf:set('__templates_supported_extensions', SupportedExtensions),
    s_conf:set('__templates_extension_map', ExtensionMap),
    ok.
    
append_extensions({Module, Extensions}, Acc) ->
    lists:foldl(fun(Ext, A) -> 
                        case gb_trees:lookup(Ext, A) of
                            {value, _} -> 
                                s_log:warn(?MODULE, "Duplicate usage for extension '~s' in template engine ~p", [Ext, Module]);
                            none -> gb_trees:insert(Ext, Module, A)
                        end
                end, Acc, Extensions).

%%%-------------------------------------------------------------------
%%% File:      erlydtl_compiler.erl
%%% @author    Alexander Borovsky <alex.borovsky@gmail.com>
%%% @copyright 2009, Alexander Borovsky
%%% @doc  
%%% ErlyDTL template renderer. It's default renderer implementation.
%%% Your framework can use your own renderer for dowsn't recompile template all times
%%% @end  
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Roberto Saccon, Evan Miller
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%% @since 2009-08-27 by Alexander Borovsky
%%%-------------------------------------------------------------------
-module(erlydtl_renderer).
-author('alex.borovsky@gmail.com').

-include("../../include/erlydtl.hrl").

%% API
-export([render/4, render/3, set_block/3, render_block/2, have_child_block/2]).

-record(renderer_params, {
          compiler_options = [],
          templates_dir = "",
          erl_out_dir = none
         }).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @spec render(string(), erlydtl_params(), any()) -> iolist()
%% @doc Renders template using default module name
%% @end
%%--------------------------------------------------------------------
-spec(render/3 :: (string(), erlydtl_params(), any()) -> iolist()).
render(Path, Params, Options) ->
    ModuleName = to_module_name(Path),
    Module = list_to_atom(ModuleName),
    render(Path, Module, Params, Options).

%%--------------------------------------------------------------------
%% @spec render(string(), atom(), erlydtl_params(), any()) -> iolist()
%% @doc Renders template using specified module name
%% @end
%%--------------------------------------------------------------------
-spec(render/4 :: (string(), atom(), erlydtl_params(), any()) -> iolist()).
render(Path, Module, Params, #renderer_params{} = RenderParams) ->
    TemplatePath = filename:join(RenderParams#renderer_params.templates_dir, 
                                 Path),
    CompileParams = compiler_params(Path, RenderParams),
    
    case erlydtl:compile(TemplatePath, Module, CompileParams) of
        ok -> 
            Module:render(Params);
        Error -> Error
    end;

render(Path, Module, Params, CompilerOptions) ->
    RendererParams = init_from_compiler_options(CompilerOptions),
    render(Path, Module, Params, RendererParams).



%%--------------------------------------------------------------------
%% @spec set_block(string(), iolist(), #renderer_params{}) -> #renderer_params{}
%% @doc Saves block value (in context)
%% @end
%%--------------------------------------------------------------------
-spec(set_block/3 :: (string(), iolist(), #renderer_params{}) -> #renderer_params{}).
set_block(BlockName, Content, Context) -> 
    erlydtl_runtime:extend_dict(Context, [{generate_block_name(BlockName), Content}]).

%%--------------------------------------------------------------------
%% @spec have_child_block(string(), erlydtl_params()) -> boolean()
%% @doc Checks if current block populated
%% @end
%%--------------------------------------------------------------------
-spec(have_child_block/2 :: (string(), erlydtl_params()) ->
             boolean()).
have_child_block(BlockName, Context) ->
    case erlydtl_runtime:find_value(generate_block_name(BlockName), Context) of
        undefined -> false;
        _ -> true
    end.


%%--------------------------------------------------------------------
%% @spec render_block(string(), erlydtl_params()) -> iolist()
%% @doc Renders block with specified name
%% @end
%%--------------------------------------------------------------------
-spec(render_block/2 :: (string(), erlydtl_params()) -> iolist()).
render_block(BlockName, Context) ->
    case erlydtl_runtime:find_value(generate_block_name(BlockName), Context) of
        undefined -> "";
        Val -> Val
    end.

%%====================================================================
%% Internal functions
%%====================================================================
compiler_params(Path, RenderParams) ->
    case RenderParams#renderer_params.erl_out_dir of
        none -> [];
        Dir -> File = filename:join(Dir, to_module_name(Path) ++ ".erl"),
               filelib:ensure_dir(File),
               [{write_erl_to, File}]
    end ++
        [{renderer_params, RenderParams} | 
                     RenderParams#renderer_params.compiler_options].

to_module_name(Path) ->
    PathNew = string:join(string:tokens(Path, "."), "_") ++ "_view",
    string:join(string:tokens(PathNew, "/"), "$") ++ "_view".

init_from_compiler_options(CompilerOptions) ->
    #renderer_params{
        compiler_options = CompilerOptions,
        templates_dir = proplists:get_value(doc_root, CompilerOptions, ""),
        erl_out_dir = proplists:get_value(erl_out_dir, CompilerOptions, none)
    }.

generate_block_name(Name) ->
    "__" ++ Name ++ "_block".

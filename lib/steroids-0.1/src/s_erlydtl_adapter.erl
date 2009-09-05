%%%-------------------------------------------------------------------
%%% File    : s_erlydtl_adapter.erl
%%% Author  : Alexander Borovsky <partizan@altlinux.ru>
%%% Description : 
%%%
%%% Created : 19 Aug 2009 by Alexander Borovsky <partizan@altlinux.ru>
%%%-------------------------------------------------------------------
-module(s_erlydtl_adapter).

%% API
-behaviour(s_template_loader).
-export([compile/2]).

-export([render/3, set_block/3, render_block/2, have_child_block/2]).

%%====================================================================
%% API
%%====================================================================

%%
%% @spec compile(string(), atom()) -> ok | {error, any()}
%% @doc Compiles ErlyDTL template to Module
%% @end
%%
-spec(compile(string(), atom()) -> ok | {error, any()}).
compile(Path, Module) ->
    File = filename:join(s_conf:get(ebin_dir), atom_to_list(Module) ++ ".erl"),
    
    erlydtl:compile(Path, Module, [{out_dir, s_conf:get(ebin_dir)},
                                   debug,
                                   {renderer_module, s_erlydtl_adapter},
                                   {write_erl_to, File}]).

%%====================================================================
%% Callbacks from ErlyDTL views
%%====================================================================

%%--------------------------------------------------------------------
%% @spec render(string(), erlydtl_params(), any()) -> {ok, iolist()}
%% @doc Renders template using default module name
%% @end
%%--------------------------------------------------------------------
-spec(render/3 :: (string(), any(), any()) -> {ok, iolist()}).
render(Path, Params, _) ->
    {ok, s_template:render(Path, Params)}.

%%--------------------------------------------------------------------
%% @spec set_block(string(), iolist(), _) -> #renderer_params{}
%% @doc Saves block value (in context)
%% @end
%%--------------------------------------------------------------------
-spec(set_block/3 :: (string(), iolist(), any()) -> any()).
set_block(BlockName, Content, Context) ->
    case s_template:have_block(BlockName) of
        true -> Context;
        false -> s_template:set_block(BlockName, Content)
    end,
    Context.

%%--------------------------------------------------------------------
%% @spec have_child_block(string(), erlydtl_params()) -> boolean()
%% @doc Checks if current block populated
%% @end
%%--------------------------------------------------------------------
-spec(have_child_block/2 :: (string(), any()) -> boolean()).
have_child_block(_BlockName, _Context) ->
    false.

%%--------------------------------------------------------------------
%% @spec render_block(string(), erlydtl_params()) -> iolist()
%% @doc Renders block with specified name
%% @end
%%--------------------------------------------------------------------
-spec(render_block/2 :: (string(), any()) -> tuple()).
render_block(BlockName, _Context) ->
    {block, BlockName}.

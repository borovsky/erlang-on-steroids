%%%-------------------------------------------------------------------
%%% @author  : Alexander Borovsky <partizan@altlinux.ru>
%%% @doc Template system
%%% @version 0.0.1
%%% @end
%%% 
%%%-------------------------------------------------------------------
-module(s_templates).

%% API
-export([render/2, append_stored/2, set_stored/2, get_stored/1]).

-include("s_types.hrl").

-define(MAX_DEEP, 20).
-define(BLOCK_NAME_PREFIX, "__block_").

%%====================================================================
%% API
%% ====================================================================


%%
%% @spec render(string(), dict()) -> iolist()
%% @doc  Renders template, specified by path. 
%% @end
%% 
-spec(render/2 :: (string(), dict()) -> iolist()).
render(TemplatePath, Params) ->
    ModuleName = s_reloader:load_thing(s_template_loader, TemplatePath),
    Result = apply(ModuleName, render, [Params]),
    process_enhanced_iolist(Result).

%%
%% @spec append_stored(atom(), extended_iolist()) -> ok
%% @doc  Appends/creates stored block of content
%% @end
%% 
-spec(append_stored/2 :: (atom(), extended_iolist()) -> ok).
append_stored(Name, Value) ->
    BlockName = calculate_stored_block_name(Name),
    Prev = case s_context:get(BlockName) of
               undefined ->
                   [];
               List when is_list(List) ->
                   List;
               Value -> [Value]
           end,
    s_context:put(BlockName, [Value | Prev]),
    ok.
            
%%
%% @spec set_stored(atom(), extended_iolist()) -> ok
%% @doc  Replaces/creates stored block of content
%% @end
%% 
-spec(set_stored/2 :: (atom(), extended_iolist()) -> ok).
set_stored(Name, Value) ->
    BlockName = calculate_stored_block_name(Name),
    s_context:put(BlockName, [Value]),
    ok.

%%
%% @spec get_stored(atom()) -> extended_iolist()
%% @doc  Fetchs stored block of content / empty list if it's not exists
%% @end
%% 
-spec(get_stored/1 :: (atom()) -> extended_iolist()).
get_stored(Name) ->
    BlockName = calculate_stored_block_name(Name),
    case s_context:get(BlockName) of
        undefined -> [];
        Value -> lists:reverse(Value)
    end.


calculate_stored_block_name(Name) ->
    list_to_atom(?BLOCK_NAME_PREFIX ++ atom_to_list(Name)).

%%====================================================================
%% Internal functions
%%====================================================================


-spec(process_enhanced_iolist/1 :: (extended_iolist()) -> iolist()).
process_enhanced_iolist(List) ->
    process_enhanced_iolist([], lists:flatten(List), ?MAX_DEEP).

-spec(process_enhanced_iolist/2 :: (extended_iolist(), integer()) -> iolist()).
process_enhanced_iolist(List, Deep) ->
    process_enhanced_iolist([], lists:flatten(List), Deep).

-spec(process_enhanced_iolist/3 :: (iolist(), extended_iolist(), integer()) -> iolist()).
process_enhanced_iolist(Out, [], _Deep) ->
    lists:reverse(Out);
process_enhanced_iolist(_Out, _List, 0) ->
    throw(too_deep_recursive_substitution);
process_enhanced_iolist(Out, [E | List], Deep) when is_atom(E) ->
    Substitution = get_substitution(E),
    Processed = process_enhanced_iolist(Substitution, Deep - 1),
    process_enhanced_iolist([Processed | Out], List, Deep);
process_enhanced_iolist(Out, [E | List], Deep) ->
    process_enhanced_iolist([E | Out], List, Deep).

get_substitution(Name) ->
    s_templates:get_stored(Name).



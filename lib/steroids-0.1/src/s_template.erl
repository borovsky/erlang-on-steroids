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
%%% @author  : Alexander Borovsky <partizan@altlinux.ru>
%%% @doc Template system
%%% @version 0.1
%%% @end
%%% 

-module(s_template).

%% API
-export([render/2, append_block/2, set_block/2, get_block/1, have_block/1]).


-include("s_types.hrl").

-define(MAX_DEEP, 20).
-define(BLOCK_NAME_PREFIX, "__block_").

%%====================================================================
%% API
%%====================================================================

%%
%% @spec render(string(), dict()) -> iolist()
%% @doc  Renders template, specified by path. 
%% @end
%% 
-spec(render/2 :: (string(), view_parameter_type()) -> iolist()).
render(TemplatePath, Params) ->
    ModuleName = s_reloader:load_thing(s_template_loader, TemplatePath),
    case ModuleName:render(Params) of
        {ok, Result} ->
            process_enhanced_iolist(Result);
        {error, Reason} ->
            s_log:error(?MODULE, "Render error: ~p", [Reason]),
            throw(Reason)
    end.

%%
%% @spec append_block(atom(), extended_iolist()) -> ok
%% @doc  Appends/creates stored block of content
%% @end
%% 
-spec(append_block/2 :: (atom(), extended_iolist()) -> ok).
append_block(Name, Value) ->
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
%% @spec set_block(atom(), extended_iolist()) -> ok
%% @doc  Replaces/creates stored block of content
%% @end
%% 
-spec(set_block/2 :: (string() | atom(), extended_iolist()) -> ok).
set_block(Name, Value) ->
    BlockName = calculate_stored_block_name(Name),
    s_context:put(BlockName, [Value]),
    ok.

%%
%% @spec get_block(atom()) -> extended_iolist()
%% @doc  Fetchs stored block of content / empty list if it's not exists
%% @end
%% 
-spec(get_block/1 :: (string() | atom()) -> extended_iolist()).
get_block(Name) ->
    BlockName = calculate_stored_block_name(Name),
    case s_context:get(BlockName) of
        undefined -> [];
        Value -> lists:reverse(Value)
    end.

%%
%% @spec have_block(atom()) -> boolean()
%% @doc  Checks if block already renderen somewere
%% @end
%% 
-spec(have_block/1 :: (string() | atom()) -> boolean()).
have_block(Name) ->
    BlockName = calculate_stored_block_name(Name),
    case s_context:get(BlockName) of
        undefined -> false;
        _ -> true
    end.


calculate_stored_block_name(Name) when is_atom(Name)->
    calculate_stored_block_name(atom_to_list(Name));
calculate_stored_block_name(Name) ->
    list_to_atom(?BLOCK_NAME_PREFIX ++ Name).

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

process_enhanced_iolist(Out, [{block, BlockName} | List], Deep) ->
    Substitution = get_substitution(BlockName),
    Processed = process_enhanced_iolist(Substitution, Deep - 1),
    process_enhanced_iolist([Processed | Out], List, Deep);

process_enhanced_iolist(Out, [E | List], Deep) when is_atom(E)->
    process_enhanced_iolist([atom_to_list(E) | Out], List, Deep);
    
process_enhanced_iolist(Out, [E | List], Deep) ->
    process_enhanced_iolist([E | Out], List, Deep).

get_substitution(Name) ->
    get_block(Name).



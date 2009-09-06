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
%%% @doc Multipart processing for inets webserver
%%% @version 0.1
%%% @private
%%% 

-module(s_multipart_inets).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("s_types.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([get_multipart/2
        ]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
        ]).

%%====================================================================
%% External functions
%%====================================================================

%%
%% @spec get_multipart(string(), string()) -> list(request_parameter())
%% @doc Parses multipart body
%%
-spec(get_multipart/2 :: (string(), string()) -> list(request_parameter())).
get_multipart(Body, Boundary) ->
    Regexp = "--" ++ Boundary ++ "((\r\n)|(\-\-\r\n))",
    BodyBin = list_to_binary(Body),
    Split = re:split(BodyBin, Regexp, [{return, binary}, group,trim]),
    retrive_data(Split).

%%====================================================================
%% Internal types
%%====================================================================
-type(parsed_http_header() :: {string(), string(), list({string(), string()})}).

%%====================================================================
%% Internal functions
%%====================================================================

%%
%% @spec retrive_data(list(list(binary()))) -> list(request_parameter())
%% @doc Generates parameters from multiparts
%%--------------------------------------------------------------------
-spec(retrive_data/1 :: (list(list(binary()))) -> list(request_parameter())).
retrive_data([]) ->
    [];
retrive_data([[] | Rest]) ->
    retrive_data(Rest);
retrive_data([[<<>> | _] | Rest]) ->
    retrive_data(Rest);
retrive_data([[Part | _] | Rest]) ->
    {match, [Header, Data]} = re:run(Part, "^(.*)\r\n\r\n(.*)\r\n\$", [{capture, [1, 2], binary}, dotall]),
    Headers = re:split(Header, "\r\n", [{return,list}]),
    ProcessedHeaders = parse_headers(Headers),
    [process_part(ProcessedHeaders, Data) | retrive_data(Rest)].

%%
%% @spec process_part(list(parsed_http_header()), binary()) -> request_parameter()
%% @doc Processes one part
%%
-spec(process_part/2 :: (list(parsed_http_header()), binary()) -> request_parameter()).
process_part(Header, Data) ->
    {value, {"content-disposition", _, Params}} = lists:keysearch("content-disposition", 1, Header),
    {value, {"name", Name}} = lists:keysearch("name", 1, Params),
    case lists:keysearch("filename", 1, Params) of
        false -> {Name, binary_to_list(Data)};
        {value, {_, FileName}} ->
            ContentType = case lists:keysearch("content-type", 1, Header) of
                              false -> "application/octet-stream";
                              {value, {_, Type, _}} -> Type
                          end,
            {Name, [{file_path, save_file(Data)},
                    {file_name, sanitize_file_name(FileName)},
                    {content_type, ContentType}]}
    end.

%%
%% @spec save_file(binary()) -> ok | error
%% @doc Stores uploaded file to temporary directory
%%
-spec(save_file/1 :: (binary()) -> ok | error).
save_file(Data) ->
    Dir = case s_conf:get(upload_dir) of
              not_found -> "/tmp";
              D -> D
          end,
    FileName = filename:join(Dir, next_file_name()),
    filelib:ensure_dir(FileName),

    Ret = file:write_file(FileName, Data),
    case Ret of 
        ok -> FileName;
        {error, Reason} ->
            io:format("Error while saving file to ~s: ~p ~n", [FileName, Reason]),
            error
        end.

%%
%% @spec next_file_name() -> string()
%% @doc Generates unique file name
%%
-spec(next_file_name/0 :: () -> string()).
next_file_name() ->
    {MegaSecs, Secs, MicroSecs} = now(),
    io_lib:format("~w-~w-~w.dat", [MegaSecs, Secs, MicroSecs]).


%%
%% @spec sanitize_file_name(string()) -> string()
%% @doc Sanitizes file name (some IE versions provides full path to file
%%
-spec(sanitize_file_name/1 :: (string()) -> string()).
sanitize_file_name(Name) ->
    filename:basename(lists:last(string:tokens(Name, "\\"))).

%%
%% @spec parse_headers([binary()]) -> list(parsed_http_header())
%% @doc  Parses HTTP-like headers to more useful form
%%
-spec(parse_headers/1 :: ([binary()]) -> list(parsed_http_header())).
parse_headers([]) -> 
    [];
parse_headers([Line|Rest]) -> 
    [parse_header(Line)|parse_headers(Rest)].

%%
%% @spec parse_header(string()) -> parsed_http_header()
%% @doc Parses one HTTP header
%%
-spec(parse_header/1 :: (string()) -> parsed_http_header()).
parse_header(Header) ->
    {match, [Name, Value]} = re:run(Header, "^(.*):(.*)\$", [{capture, [1, 2], list}]),
    [HeaderValue | Params] = lists:map(fun string:strip/1, re:split(Value, ";", [{return, list}])),
    
    MappedParams = lists:map(fun(S) -> 
                                     {match, [N, R]} = re:run(S, "^(.*)=\"(.*)\"\$", 
                                                              [{capture, [1, 2], list}]),
                                     {N, R}
                                     end, Params),
    {string:to_lower(Name), HeaderValue, MappedParams}.

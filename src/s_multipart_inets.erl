%%%-------------------------------------------------------------------
%%% File    : s_multipart_inets.erl
%%% Author  : Alexander Borovsky <alex@partizan.home>
%%% Description : 
%%%
%%% Created : 26 Jun 2009 by Alexander Borovsky <alex@partizan.home>
%%%-------------------------------------------------------------------
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
%%--------------------------------------------------------------------
%% Function: Parses multipart body
%% Description:
%%--------------------------------------------------------------------

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

%%--------------------------------------------------------------------
%% Function: Generates parameters from multiparts
%% Description:
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

%%--------------------------------------------------------------------
%% Function: Processes one part
%% Description:
%%--------------------------------------------------------------------
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

-spec(save_file/1 :: (binary()) -> ok | error).
save_file(Data) ->
    FileName = "/tmp/" ++ next_file_name(),

    Ret = file:write_file(FileName, Data),
    case Ret of 
        ok -> FileName;
        {error, Reason} ->
            io:format("Error while saving file to ~s: ~p ~n", [FileName, Reason]),
            error
        end.

-spec(next_file_name/0 :: () -> string()).
next_file_name() ->
    "1".

-spec(sanitize_file_name/1 :: (string()) -> string()).
sanitize_file_name(Name) ->
    Name.

%%--------------------------------------------------------------------
%% Function: Parses HTTP-like headers to more useful form
%% Description:
%%--------------------------------------------------------------------
-spec(parse_headers/1 :: ([binary()]) -> list(parsed_http_header())).
parse_headers([]) -> 
    [];
parse_headers([Line|Rest]) -> 
    [parse_header(Line)|parse_headers(Rest)].

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

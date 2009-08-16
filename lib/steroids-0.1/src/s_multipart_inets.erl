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
%% Function: retrive_data/1
%% Description: Generates parameters from multiparts
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

%%--------------------------------------------------------------------
%% Function: save_file/1
%% Description: Stores file to upload directory
%%--------------------------------------------------------------------
-spec(save_file/1 :: (binary()) -> ok | error).
save_file(Data) ->
    Dir = case s_conf:get(upload_dir) of
              not_found -> "/tmp";
              D -> D
          end,
    s_utils:mkdir_p(Dir),
    FileName = filename:join(Dir, next_file_name()),

    Ret = file:write_file(FileName, Data),
    case Ret of 
        ok -> FileName;
        {error, Reason} ->
            io:format("Error while saving file to ~s: ~p ~n", [FileName, Reason]),
            error
        end.

%%--------------------------------------------------------------------
%% Function: next_file_name/0
%% Description: Generates unique file name
%%--------------------------------------------------------------------
-spec(next_file_name/0 :: () -> string()).
next_file_name() ->
    {MegaSecs, Secs, MicroSecs} = now(),
    io_lib:format("~w-~w-~w.dat", [MegaSecs, Secs, MicroSecs]).


%%--------------------------------------------------------------------
%% Function: sanitize_file_name/1
%% Description: Sanitizes file name (some IE versions provides full path to file
%%--------------------------------------------------------------------
-spec(sanitize_file_name/1 :: (string()) -> string()).
sanitize_file_name(Name) ->
    lists:last(string:tokens(Name, "\\")).

%%--------------------------------------------------------------------
%% Function: parse_headers/1
%% Description:  Parses HTTP-like headers to more useful form
%%--------------------------------------------------------------------
-spec(parse_headers/1 :: ([binary()]) -> list(parsed_http_header())).
parse_headers([]) -> 
    [];
parse_headers([Line|Rest]) -> 
    [parse_header(Line)|parse_headers(Rest)].

%%--------------------------------------------------------------------
%% Function: parse_header/1
%% Description:  Parses one HTTP header
%%--------------------------------------------------------------------
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
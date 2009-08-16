%%%-------------------------------------------------------------------
%%% File    : s_platform_inets.erl
%%% Author  : Alexander Borovsky <alex@partizan.home>
%%% Description : 
%%%
%%% Created : 24 Jun 2009 by Alexander Borovsky <alex@partizan.home>
%%%-------------------------------------------------------------------
-module(s_platform_inets).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("s_internal_types.hrl").
-include_lib("inets/src/httpd.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([do/1]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([is_request_processed/1,
         parse_get_args/1,
         parse_post_args/1
        ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: do
%% Description: processes request from inets
%%--------------------------------------------------------------------
-spec(do/1 :: (#mod{}) -> tuple()).
do(#mod{} = A) ->
    case is_request_processed(A#mod.data) of
        true ->    
            {proceed, A#mod.data};
        false -> try do_process_request(A)
                     catch Type : Error ->
                      io:format("CAUGHT ERROR: ~p-~p~n~p~n", [Type, Error, erlang:get_stacktrace()]),
                      {proceed, A#mod.data}
              end
end.

%%--------------------------------------------------------------------
%% Function: is_request_processed
%% Description: Checks, if request already processed
%%--------------------------------------------------------------------
-spec(is_request_processed(string()) -> bool()).
is_request_processed([{response, {already_sent, _, _}}, _]) ->
    true;
is_request_processed(_) ->
    false.

%%--------------------------------------------------------------------
%% Function: get_querystring/1
%% Description: Extracts query string from 
%%--------------------------------------------------------------------
-spec(get_querystring/1 :: (string()) -> {string(), string()}).
get_querystring(Uri) ->
    R = httpd_util:split_path(Uri),
    {Path, QueryString} = R,
    case QueryString of 
        [] -> {Path, []}; 
        QueryString -> {Path, tl(QueryString)}
    end.


-spec(parse_query_string/1 :: (string()) -> list({string(), string()})).
parse_query_string(String) ->
    Query = httpd:parse_query(String),
    [{Key, Value} || {Key, Value} <- Query, Key /= []].
    

-spec(parse_get_args/1 :: (#mod{}) ->  {string(), list(tuple())}).
parse_get_args(Info) ->
    {Path, QueryString} = get_querystring(Info#mod.request_uri),
    {Path, parse_query_string(QueryString)}.

-spec(parse_post_args/1 :: (#mod{}) ->  list(tuple())).
parse_post_args(Info) ->
    BoundaryStruct = fetch_boundary(Info),
    case BoundaryStruct of
        {simple, Data} ->
            parse_query_string(Data);
        {multipart, Boundary} ->
            s_multipart_inets:get_multipart(Info#mod.entity_body, Boundary)
    end.



-spec(fetch_boundary/1 :: (#mod{parsed_header::maybe_improper_list()}) -> {'multipart',_} | {'simple',_}).
fetch_boundary(Data) ->
    ContentTypeSearch = lists:keysearch("content-type", 1, Data#mod.parsed_header),
    ContentType = case ContentTypeSearch of
                      {value, {_, Type}} -> Type;
                      _ -> other                                            
                  end,
    case ContentType of
        "multipart/form-data; boundary=" ++ Boundary -> 
            {multipart, Boundary};
        _ -> 
            {simple, Data#mod.entity_body}
    end.

%%====================================================================
%% Internal functions
%%====================================================================
-spec(do_process_request/1 :: (#mod{}) -> tuple()).
do_process_request(Info) ->
    {Path, GetParams} = parse_get_args(Info),
    PostParams = parse_post_args(Info),
    Method = map_method(Info#mod.method),
    Request = #common_request_record{
      method = Method, 
      url = Path,
      get_params = GetParams,
      post_params = PostParams
     },
    Response = process_response(s_dispatcher:dispatch(Request), Method),
    Response.

-spec(process_response/2 :: (steroids_response(), atom()) -> tuple()).
process_response(#render_response{data = Body, 
                                  content_type = ContentType,
                                  status_code = ResponseCode
                                 }, Method) ->

    Size = integer_to_list(iolist_size(Body)),

    Headers = lists:flatten([
                             {code, ResponseCode},
                             {content_type, ContentType},
                             {content_length, Size}
                            ]),		


    ResponsePre = case Method of
                   head -> {response, {response, Headers, nobody}};
                   _ -> {response, {response, Headers, Body}}
               end,

    {proceed,[ ResponsePre ]};
process_response(#redirect_response{target = Target}, _Method) ->
    {proceed,
     [{response,
       {301, ["Location: ", Target, "\r\n"
              "Content-Type: text/html\r\n",
              "\r\n",
              "<HTML>\n<HEAD>\n<TITLE>Redirect</TITLE>\n</HEAD>\n",
              "<BODY>\n</BODY>\n</HTML>\n"]}}]}.

-spec(map_method/1 :: (string()) -> atom()).
map_method("POST") ->
    post;
map_method("HEAD") ->
    head;
map_method(_) ->
    get.
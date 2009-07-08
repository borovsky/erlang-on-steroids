%%%-------------------------------------------------------------------
%%% File    : e_platform_inets.erl
%%% Author  : Alexander Borovsky <alex@partizan.home>
%%% Description : 
%%%
%%% Created : 24 Jun 2009 by Alexander Borovsky <alex@partizan.home>
%%%-------------------------------------------------------------------
-module(my_platform_inets).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
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
do(#mod{} = A) ->
    io:format("Request: ~p~n", [A]),
    io:format("Request data: ~p~n", [A#mod.data]),
    
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
-spec(is_request_processed(string()) ->
             bool()).
is_request_processed([{response, {already_sent, _, _}}, _]) ->
    true;
is_request_processed(_) ->
    false.

get_querystring(Uri) ->
    R = httpd_util:split_path(Uri),
    {_Path, QueryString} = R,
    case QueryString of 
        [] -> []; 
        _ -> tl(QueryString) 
    end.


parse_query_string(String) ->
    Query = httpd:parse_query(String),
    [{Key, Value} || {Key, Value} <- Query, Key /= []].
    

parse_get_args(Info) ->
    QueryString = get_querystring(Info#mod.request_uri),
    parse_query_string(QueryString).

parse_post_args(Info) ->
    BoundaryStruct = fetch_boundary(Info),
    case BoundaryStruct of
        {simple, Data} ->
            parse_query_string(Data);
        {multipart, Boundary} ->
            my_multipart_inets:get_multipart(Info#mod.entity_body, Boundary)
    end.

-spec(fetch_boundary/1 :: (list()) -> {simple, string()} | {multipart, string()}).	     
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


do_process_request(Info) ->
    GetParams = parse_get_args(Info),
    PostParams = parse_post_args(Info),
    io:format("GET params: ~p~n", [GetParams]),
    io:format("POST params: ~p~n", [PostParams]),
    ResponseCode = 200,
    ContentType = "text/html",
    Body = "<html><head><title>Test</title></head><body><h1>Test</h1></body></html>",
    Size = integer_to_list(httpd_util:flatlength(Body)),

    Headers = lists:flatten([
                             {code, ResponseCode},
                             {content_type, ContentType},
                             {content_length, Size}
                            ]),		


    ResponsePre = case Info#mod.method of
                   "HEAD" -> {response, {response, Headers, nobody}};
                   _ -> {response, {response, Headers, Body}}
               end,

    Response = {proceed,[ ResponsePre
                  ]},
    io:format("Response: ~p~n", [Response]),
    Response. 

%%====================================================================
%% Internal functions
%%====================================================================

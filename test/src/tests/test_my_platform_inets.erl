-module(test_my_platform_inets).

-author("alex.borovsky@gmail.com").

-behaviour(erunit_test).
-export([tests/0]).

-import(erunit, [test/2, assertEquals/2]).

already_processed_request() ->
    RequestData=[{response,{already_sent,200,10360}},{mime_type,"text/html"}],
    my_platform_inets:is_request_processed(RequestData).

not_processed_request() ->
    RequestData=[{status,{500,none,"httpd_file: Can't openpublic/"}}],
    my_platform_inets:is_request_processed(RequestData).

parse_get_request(RequestPath) ->
    Request = {mod,{init_data,{53597,"127.0.0.1"},"localhost"},
               [], ip_comm, port, httpd_conf__0_0_0_0__4000, "GET",
               "127.0.0.1:4000" ++ RequestPath, RequestPath,"HTTP/1.0",
               "GET " ++ RequestPath ++ " HTTP/1.0",
               [{"connection","Keep-Alive"},
                {"host","127.0.0.1:4000"},
                {"accept","*/*"},
                {"user-agent","Wget/1.11.4"}],
               [],false},
    my_platform_inets:parse_get_args(Request).

check_get_request(Title, Params, Uri) ->
    test(Title, fun() ->
                        assertEquals(Params, parse_get_request(Uri))
                end).

post_boundary() ->
    "---------------------------4496613113728428681934704695".

gen_post_request(Encoding, PostBody) ->
    Boundary = post_boundary(),
    ContentType = case Encoding of
                   url -> "application/x-www-form-urlencoded";
                   _ -> "multipart/form-data; boundary=" ++ Boundary
               end,
    RequestPath = "/test",
    Request = {mod,{init_data,{53597,"127.0.0.1"},"localhost"},
               [], ip_comm, port, httpd_conf__0_0_0_0__4000, "POST",
               "127.0.0.1:4000" ++ RequestPath, RequestPath, "HTTP/1.1",
               "POST "++ RequestPath ++" HTTP/1.1",
               [{"connection","Keep-Alive"},
                {"host","127.0.0.1:4000"},
                {"accept","*/*"},
                {"user-agent","Wget/1.11.4"},
                {"content-type", ContentType}],
               PostBody, false},
    Request.
    

parse_post_request(Request) ->
    my_platform_inets:parse_post_args(Request).

check_post_request(Name, Result, Encoding, PostBody) ->
    Request = gen_post_request(Encoding, PostBody),
    test(Name, fun() -> 
                       assertEquals(Result, parse_post_request(Request))
               end).

request_processed_test() ->
    [test("Request processed test should return true for processed request", 
          fun() -> assertEquals(true, already_processed_request()) end),
     test("Request processed test should return true for processed request", 
          fun() -> assertEquals(false, not_processed_request()) end)
    ].    

get_request_parse_test() ->
    [check_get_request("Request to root with no query", [], "/"),
     check_get_request("Request to page with no query", [], "/test.html"),
     check_get_request("Request to page with empty query", [], "/test.html?"),
     check_get_request("Request to page with one parameter", 
                       [{"test", "abc"}], "/test.html?test=abc"),
     check_get_request("Request to page with two parameters", 
                       [{"test", "abc"}, {"test2", "cde"}], "/test.html?test=abc&test2=cde"),
     check_get_request("Request to page with one parameter with plus", 
                       [{"test", "abc cde"}], "/test.html?test=abc+cde"),
     check_get_request("Request to page with one parameter (quoted chars)", 
                       [{"test", "abc cde"}], "/test.html?test=abc%20cde")
    ].    

parse_post_url_encoded_test() ->
    [ 
      check_post_request("Empty url encoded POST", [], url, ""),
      check_post_request("One url encoded POST", [{"a", "b"}], url, "a=b"),
      check_post_request("One url encoded POST", [{"a", "b"}, {"c", "d"}], url, "a=b&c=d"),
      check_post_request("One url encoded POST", [{"a", "b d"}], url, "a=b+d"),
      check_post_request("One url encoded POST", [{"a", "b d"}], url, "a=b%20d")
    ].

encode_multipart_parameter({Name, {file, FileParams}}) ->
    FileName = param(file_name, FileParams),
    Disposition = "Content-Disposition: form-data; name=\"" ++ Name ++"\"; filename=\"" ++ FileName++ "\"\r\n",
    ContentType = "Content-Type: " ++ param(content_type, FileParams) ++ "\r\n\r\n",
    Disposition ++ ContentType ++ param(content, FileParams) ++ "\r\n";
encode_multipart_parameter({Name, Value}) ->
    "Content-Disposition: form-data; name=\"" ++ Name ++"\"\r\n\r\n" ++ Value ++ "\r\n".

gen_multipart_post([]) ->
     "";
gen_multipart_post(Parameters) ->
     gen_multipart_post([post_boundary(), "--"], Parameters).

gen_multipart_post(Chunks, []) ->
    lists:flatten(lists:reverse(["--\r\n" | Chunks]));
gen_multipart_post(Chunks,[T | V]) ->
    gen_multipart_post([post_boundary(), "--", encode_multipart_parameter(T), "\r\n" | Chunks], V).

param(Key, List) ->
    case lists:keysearch(Key, 1, List) of
        {value, {_, Value}} -> Value;
        _ -> false
    end.
            

check_multipart_post_request(Name, Result) ->
    PostBody = gen_multipart_post(Result),
    check_post_request(Name, Result, multipart, PostBody).

check_file_post_request(Name, Variables) ->
    PostBody = gen_multipart_post(Variables),
    Request = gen_post_request(multipart, PostBody),
    test(Name, fun () ->
                       Res = parse_post_request(Request),
                       [{FieldName, SrcParams}] = Variables,
                       [{FieldName, Params}] = Res,
                       {file, FileSrcParams} = SrcParams,
                       assertEquals(param(file_name, FileSrcParams), param(file_name, Params)),
                       assertEquals(param(content_type, FileSrcParams), param(content_type, Params)),
                       {ok, FileContent} = file:read_file(param(file_path, Params)),
                       assertEquals(list_to_binary(param(content, FileSrcParams)), FileContent),
                       1
               end).

parse_post_multipart_test() ->
    [
     check_multipart_post_request("Empty multipart POST", []),
     check_multipart_post_request("Multipart POST with one simple parameter", [{"a", "b"}]),
     check_multipart_post_request("Multipart POST with two simple parameters", [{"a", "b"}, {"c", "d"}]),
     check_multipart_post_request("Multipart POST with parameter with space", [{"a", "b d"}]),

     check_file_post_request("Multipart POST with file", 
                             [{"a", {file, [{content, "abcd"}, {file_name, "test.txt"}, {content_type, "text/plain"}]}}])
    ].

tests() ->
    lists:flatten([get_request_parse_test(),
                   parse_post_url_encoded_test(),
                   parse_post_multipart_test(),
                   request_processed_test()]).

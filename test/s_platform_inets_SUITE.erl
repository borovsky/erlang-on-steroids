-module(s_platform_inets_SUITE).

-author("alex.borovsky@gmail.com").

-compile(export_all).
-include_lib("ct.hrl").

init_per_suite(Config) ->
    s_conf:start(),
    Config.
end_per_suite(_Config) ->
    s_conf:terminate(),
    ok.
init_per_testcase(_TestCase, Config) ->
    Config.
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() -> 
    [check_get_to_root_with_no_params,
     check_get_with_no_params,
     check_get_with_empty_params,
     check_get_with_one_simple_parameter,
     check_get_with_two_simple_parameters,
     check_get_with_plus_parameter,
     check_get_with_encoded_parameter,
     check_simple_post_empty,
     check_simple_post_with_one_paramerer,
     check_simple_post_with_two_paramerers,
     check_simple_post_with_plus_paramerer,
     check_simple_post_with_quoted_paramerer,
     check_empty_multipart_post,
     check_multipart_post_with_one_parameter,
     check_multipart_post_with_two_parameters,
     check_multipart_post_with_non_trivial_parameters,
     check_multipart_post_with_file_parameters
    ].


already_processed_request() ->
    RequestData=[{response,{already_sent,200,10360}},{mime_type,"text/html"}],
    s_platform_inets:is_request_processed(RequestData).

not_processed_request() ->
    RequestData=[{status,{500,none,"httpd_file: Can't openpublic/"}}],
    s_platform_inets:is_request_processed(RequestData).

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
    s_platform_inets:parse_get_args(Request).

check_get_request(Params, Uri) ->
    Params =  parse_get_request(Uri).

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
    s_platform_inets:parse_post_args(Request).

check_post_request(Result, Encoding, PostBody) ->
    Request = gen_post_request(Encoding, PostBody),
    Result = parse_post_request(Request).

request_processed_test() ->
    true =  already_processed_request(),
    false = not_processed_request().    


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


check_multipart_post_request(Result) ->
    PostBody = gen_multipart_post(Result),
    check_post_request(Result, multipart, PostBody).

check_file_post_request(Variables) ->
    PostBody = gen_multipart_post(Variables),
    Request = gen_post_request(multipart, PostBody),

    Res = parse_post_request(Request),
    [{FieldName, SrcParams}] = Variables,
    [{FieldName, Params}] = Res,
    {file, FileSrcParams} = SrcParams,
    SrcFileName = param(file_name, FileSrcParams),
    SrcFileName =  param(file_name, Params),
    SrcFileType = param(content_type, FileSrcParams),
    SrcFileType = param(content_type, Params),
    {ok, FileContent} = file:read_file(param(file_path, Params)),
    FileContent = list_to_binary(param(content, FileSrcParams)),
    1.


                                                % GET request tests

check_get_to_root_with_no_params(_Config) ->
    check_get_request({"/", []}, "/").

check_get_with_no_params(_Config) ->
    check_get_request({"/test.html", []}, "/test.html").

check_get_with_empty_params(_Config) ->
    check_get_request({"/test.html", []}, "/test.html?").

check_get_with_one_simple_parameter(_Config) ->
    check_get_request({"/test.html", [{"test", "abc"}]}, "/test.html?test=abc").

check_get_with_two_simple_parameters(_Config) ->
    check_get_request({"/test.html", [{"test", "abc"}, {"test2", "cde"}]}, 
                      "/test.html?test=abc&test2=cde").

check_get_with_plus_parameter(_Config) ->
    check_get_request({"/test.html", [{"test", "abc cde"}]}, 
                      "/test.html?test=abc+cde").

check_get_with_encoded_parameter(_Config) ->
    check_get_request({"/test.html", [{"test", "abc cde"}]}, 
                      "/test.html?test=abc%20cde").

                                                % Simple POST request tests

check_simple_post_empty(_Config) ->
    check_post_request([], url, "").

check_simple_post_with_one_paramerer(_Config) ->
    check_post_request([{"a", "b"}], url, "a=b").

check_simple_post_with_two_paramerers(_Config) ->
    check_post_request([{"a", "b"}, {"c", "d"}], url, "a=b&c=d").

check_simple_post_with_plus_paramerer(_Config) ->
    check_post_request([{"a", "b d"}], url, "a=b+d").

check_simple_post_with_quoted_paramerer(_Config) ->
    check_post_request([{"a", "b d"}], url, "a=b%20d").

check_empty_multipart_post(_Config) ->
    check_multipart_post_request([]).    

check_multipart_post_with_one_parameter(_Config) ->
    check_multipart_post_request([{"a", "b"}]).

check_multipart_post_with_two_parameters(_Config) ->
    check_multipart_post_request([{"a", "b"}, {"c", "d"}]).

check_multipart_post_with_non_trivial_parameters(_Config) ->
    check_multipart_post_request([{"a", "b d"}]).

check_multipart_post_with_file_parameters(_Config) ->
    check_file_post_request([{"a", {file, [{content, "abcd"}, 
                                           {file_name, "test.txt"}, 
                                           {content_type, "text/plain"}]}}]).

-module(test_controller).

-export([index/1, hello/1]).

index(_Params) ->
    ok.

hello(_Params) ->
    Name = case s_context:get_param("name") of
               none -> "Anonymous";
               "Hacker" -> "OMG! Hacker!";
               Val -> Val
           end,
    s_context:put(name, Name),
    ok.

-module(test_controller).

-export([index/1, hello/1]).

index(_Params) ->
    ok.

hello(Params) ->
    Name = case gb_trees:lookup("name") of
               none -> "Anonymous";
               "Hacker" -> "OMG! Hacker!";
               Val -> Val
           end,
    s_context:put("name"),
    ok.

-module(main_controller).

-export([index/1]).

index(_Params) ->
    {redirect, "/test"}.

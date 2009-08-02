%%%-------------------------------------------------------------------
%%% File    : types.hrl
%%% Author  : Alexander Borovsky <partizan@altlinux.ru>
%%% Description : 
%%%
%%% Created : 11 Jul 2009 by Alexander Borovsky <partizan@altlinux.ru>
%%%-------------------------------------------------------------------

-ifndef(s_types).
-define(s_types, ok).

-record(file_request_parameter,
      {file_path :: string(),
       file_name :: string(),
       content_type ::string()
      }).

-type(request_parameter() :: {string(), string()} | {string(), #file_request_parameter{}}).
-type(request_parameters_list() :: list(request_parameter())).

-type(steroids_controller_result() :: ok | {render, string()} | {render, string() | atom(), string() | atom()}).

-type(extended_iolist() :: [any()] | binary()).

-endif.

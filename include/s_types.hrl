%%%-------------------------------------------------------------------
%%% File    : types.hrl
%%% Author  : Alexander Borovsky <partizan@altlinux.ru>
%%% Description : 
%%%
%%% Created : 11 Jul 2009 by Alexander Borovsky <partizan@altlinux.ru>
%%%-------------------------------------------------------------------

-record(file_request_parameter,
      {file_path :: string(),
       file_name :: string(),
       content_type ::string()
      }).

-type(request_parameter() :: {string(), string()} | {string(), #file_request_parameter{}}).


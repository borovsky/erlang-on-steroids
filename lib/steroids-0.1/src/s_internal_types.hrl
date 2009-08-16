%%%-------------------------------------------------------------------
%%% File    : s_internal_types.hrl
%%% Author  : Alexander Borovsky <partizan@altlinux.ru>
%%% Description : Internal types of steroid library
%%%
%%% Created : 12 Jul 2009 by Alexander Borovsky <partizan@altlinux.ru>
%%%-------------------------------------------------------------------

-ifndef(s_internal_types).
-define(s_internal_types, ok).

-include("s_types.hrl").

-record(common_request_record,
      {method :: atom(),
       url :: string(),
       get_params :: request_parameters_list(),
       post_params ::request_parameters_list()
      }).

-record(dispatched_request_record,
      {method :: atom(),
       controller ::  string(),
       action :: string(),
       parameters :: dict()
      }).

-record(redirect_response, {target :: string()}).
-record(render_response, {
          data :: iolist(),
          content_type :: string(),
          status_code :: integer() | success | not_found
         }).

-type(steroids_response() :: #redirect_response{} | 
      #render_response{}).

-type(routing_result() :: {string(), string(), request_parameters_list()} | not_found).

-endif.

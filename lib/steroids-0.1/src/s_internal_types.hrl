%%%
%%% This Library is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU Library General Public License as
%%% published by the Free Software Foundation; either version 3 of the
%%% License, or (at your option) any later version.
%%%
%%% This Library is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% Library General Public License for more details.
%%%
%%% You should have received a copy of the GNU Library General Public
%%% License along with the Gnome Library; see the file COPYING.LIB.  If not,
%%% write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
%%% Boston, MA 02111-1307, USA.
%%%

%%%
%%% @author Alexander Borovsky <alex.borovsky@gmail.com>
%%% @doc Internal types of steroid library
%%%

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
       parameters :: gb_tree()
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

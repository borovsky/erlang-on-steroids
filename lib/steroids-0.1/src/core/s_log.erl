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
%%% @author Alexander Borovsky <partizan@altlinux.ru>
%%% @doc Logger for Steroids
%%% @end
%%%
-module(s_log).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(MESSAGE_POLL, 1000).

%% API
-export([log/4, trace/3, debug/3, info/3, warn/3, error/3, fatal/3]).
-export([start_link/0, start/0, flush/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%% Internal (for tests, etc)
-export([terminate/0]).

-record(state, {
          file_id :: term(),
          message_count = 0 :: integer(),
          messages = [] :: list(),
          timer :: term()
         }).


%%====================================================================
%% API
%%====================================================================

%%
%% @spec trace(atom(), string(), list()) -> ok | skipped
%% @doc Logs trace message
%% @equiv s_log:log(trace, Module, Message, Params)
%% @end
%%
-spec(trace(atom(), string(), list()) -> ok | skipped).
trace(Module, Message, Params) ->
    log(trace, Module, Message, Params).

%%
%% @spec debug(atom(), string(), list()) -> ok | skipped
%% @doc Logs debug message
%% @equiv s_log:log(debug, Module, Message, Params)
%% @end
%%
-spec(debug(atom(), string(), list()) -> ok | skipped).
debug(Module, Message, Params) ->
    log(debug, Module, Message, Params).

%%
%% @spec info(atom(), string(), list()) -> ok | skipped
%% @doc Logs info message
%% @equiv s_log:log(info, Module, Message, Params)
%% @end
%%
-spec(info(atom(), string(), list()) -> ok | skipped).
info(Module, Message, Params) ->
    log(info, Module, Message, Params).

%%
%% @spec warn(atom(), string(), list()) -> ok | skipped
%% @doc Logs warning message
%% @equiv s_log:log(warn, Module, Message, Params)
%% @end
%%
-spec(warn(atom(), string(), list()) -> ok | skipped).
warn(Module, Message, Params) ->
    log(warn, Module, Message, Params).

%%
%% @spec error(atom(), string(), list()) -> ok | skipped
%% @doc Logs error message
%% @equiv s_log:log(error, Module, Message, Params)
%% @end
%%
-spec(error(atom(), string(), list()) -> ok | skipped).
error(Module, Message, Params) ->
    log(error, Module, Message, Params).

%%
%% @spec fatal(atom(), string(), list()) -> ok | skipped
%% @doc Logs fatal error message
%% @equiv s_log:log(fatal, Module, Message, Params)
%% @end
%%
-spec(fatal(atom(), string(), list()) -> ok | skipped).
fatal(Module, Message, Params) ->
    log(fatal, Module, Message, Params).

%%
%% @spec log(atom(), atom(), string(), list()) -> ok | skipped
%% @doc Logs message
%% @end
%%
-spec(log(atom(), atom(), string(), list()) -> ok | skipped).
log(Priority, Module, Message, Params) ->
    LogLevel = case s_context:get('__log_level') of
                   undefined -> 
                       Level = convert_log_level(s_conf:get(log_level)),
                       s_context:put('__log_level', Level),
                       Level;
                   Level -> Level
               end,
    ConvertedPriority = convert_log_level(Priority),
    if
        (ConvertedPriority >= LogLevel) -> 
            post_log_message(Priority, Module, Message, Params);
        true -> skipped
    end.    

%%
%% @spec flush() -> ok
%% @doc Force log flushing
%% @end
%%
-spec(flush() -> ok).
flush() ->
    gen_server:call(?SERVER, flush).

%%--------------------------------------------------------------------
%% @spec start() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
-spec(start/0 :: () -> {ok, pid()} | ignore | {error, any()}).
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
-spec(start_link/0 :: () -> {ok, pid()} | ignore | {error, any()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% Internal API
%%====================================================================

%%
%% @doc Terminates server (for tests only)
%% @private
%% @end
%%
-spec(terminate/0 :: () -> ok).
terminate() ->
    gen_server:call(?SERVER, terminate).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server
%% @end
%%--------------------------------------------------------------------
-spec(init/1 :: (any()) -> {ok, any()} | {stop, any()}).
init([]) ->
    LogFile = s_conf:get(log_file),
    filelib:ensure_dir(LogFile),
    case file:open(LogFile, [append, raw]) of
        {ok, IoDevice} -> 
            Timer = timer:apply_interval(1000, ?MODULE, flush, []),
            {ok, #state{file_id = IoDevice, timer = Timer}};
        {error, Reason} -> {stop, Reason}
    end.


%%--------------------------------------------------------------------
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Handling call messages
%%--------------------------------------------------------------------
-spec(handle_call/3 :: (any(), pid(), #state{}) -> tuple()).
handle_call(flush, _From, State) ->
    {reply, ok, flush_messages(State)};
handle_call(terminate, _From, State) ->
    {stop, shutdown, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Handling cast messages
%% 
-spec(handle_cast/2 :: (any(), any()) -> tuple()).
handle_cast({log, Priority, Module, FormattedMessage}, State) ->
    NewState = write_to_log([s_utils:iso_8601_fmt(erlang:localtime()), " ", priority_to_string(Priority), " ", 
                          atom_to_list(Module), ": ", FormattedMessage, io_lib:nl()], State),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% 
-spec(handle_info/2 :: (any(), any()) -> tuple()).
handle_info(_Info, State) ->
    {noreply, State}.

%% 
%% @spec terminate(Reason, State) -> void()
%% 
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end
%% 
-spec(terminate/2 :: (any(), any()) -> ok).
terminate(_Reason, #state{file_id = FileId, timer = Timer}) ->
    file:close(FileId),
    timer:cancel(Timer),
    ok.

%% 
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% 
-spec(code_change/3 :: (any(), any(), any()) -> {ok, any()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% 
%% @spec convert_log_level(atom()) -> integer()
%% @doc Convert log level to integer log priority
%% @private
%% 
-spec(convert_log_level(atom()) -> integer()).
convert_log_level(trace) -> 0;
convert_log_level(debug) ->20;
convert_log_level(info) -> 50;
convert_log_level(warn) -> 60;
convert_log_level(error) -> 80;
convert_log_level(fatal) -> 100;
convert_log_level(_) -> 0.

%% 
%% @spec post_log_message(atom(), atom(), string(), list()) -> ok
%% @doc Sends log message to logger
%% @private
%% 
-spec(post_log_message/4 :: (atom(), atom(), string(), list()) ->
             ok).
post_log_message(Priority, Module, Message, Params) ->
    FormattedMessage = list_to_binary(io_lib:format(Message, Params)),
    Cmd = {log, Priority, Module, FormattedMessage},
    gen_server:cast(?SERVER, Cmd),
    ok.

%% 
%% @spec priority_to_string(atom()) -> string()
%% @doc Convertes log message priorities to string
%% @private
%% 
-spec(priority_to_string(atom()) -> string()).
priority_to_string(trace) -> "TRACE";
priority_to_string(debug) -> "DEBUG";
priority_to_string(info) -> "INFO ";
priority_to_string(warn) -> "WARN ";
priority_to_string(error) -> "ERROR";
priority_to_string(fatal) -> "FATAL";
priority_to_string(_) -> "UNKNW".

%% 
%% @spec write_to_log(iolist(), #state{}) -> #state{}
%% @doc Writes messages to log
%% @private
%% 
-spec(write_to_log/2 :: (iolist(), #state{}) -> #state{}).
write_to_log(Message, #state{messages = Messages, message_count = Count} = State) ->
    PreparedMessage = iolist_to_binary(Message),
    NewState = State#state{message_count = Count + 1, 
                           messages = [PreparedMessage | Messages]},
    case NewState#state.message_count of
        ?MESSAGE_POLL -> flush_messages(NewState); 
        _ -> NewState
    end.

flush_messages(State) ->
    Messages = lists:reverse(State#state.messages),
    file:write(State#state.file_id, Messages),
    State#state{messages = [], message_count = 0 }.

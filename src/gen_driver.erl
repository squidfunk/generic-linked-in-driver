%% Copyright (c) 2012-2014 Martin Donath <md@struct.cc>

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to
%% deal in the Software without restriction, including without limitation the
%% rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
%% sell copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%% IN THE SOFTWARE.

-module(gen_driver).
-author('Martin Donath <md@struct.cc>').

% Functions demanded by the gen_server behaviour.
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

% Public functions.
-export([start_link/2]).

%% ----------------------------------------------------------------------------
%% Records
%% ----------------------------------------------------------------------------

% The state record for the generic driver specifies the name of the shared
% library associated with the driver, as well as the opened port.
-record(state, {
  name :: string(),
  port :: port()
}).

%% ----------------------------------------------------------------------------
%% Callback functions for gen_server behaviour
%% ----------------------------------------------------------------------------

% The driver actually exists, as this was checked in start_link/3. Therefore,
% we open the port and set it to binary mode for efficient transmission. Then
% we initialize all async threads.
init(State = #state{ name = Name }) ->
  NewState = State#state{ port = open_port({ spawn, Name }, [binary]) },
  [ begin
      port_control(NewState#state.port, (1 bsl 30) - 1, term_to_binary([])),
      ok = wait_result(NewState#state.port)
    end ||
    _ <- lists:seq(1, erlang:system_info(thread_pool_size)) ],
  { ok, NewState }.

% Perform a synchronous call on the port driver with the provided command and
% data and wait for the response in order to decode and return the result.
% If the resulting binary does not represent a valid Erlang term, decoding it
% will result in an exception that is catched. In this case return an error.
handle_call({ port, Cmd }, From, State) ->
  handle_call({ port, Cmd, [] }, From, State);
handle_call({ port, Cmd, Data }, _From, State = #state{ port = Port }) ->
  Result = case binary_to_term(port_control(Port, Cmd, term_to_binary(Data))) of
    ok ->
      wait_result(Port);
    { error, Error } ->
      { error, Error }
  end,
  { reply, Result, State };

% No other synchronous calls to be handled.
handle_call(Request, _From, State) ->
  { stop, { unknown_call, Request }, State }.

% Perform an asynchronous call on the port driver with the provided command
% and data and return immediately, ignoring any errors. The port driver must
% know about the call being asynchronous, as it is not allowed to return
% anything, so the protocol dictates to set the 30th bit to 1.
handle_cast({ port, Cmd }, State) ->
  handle_cast({ port, Cmd, [] }, State);
handle_cast({ port, Cmd, Data }, State = #state{ port = Port }) ->
  port_control(Port, Cmd bor (1 bsl 30), term_to_binary(Data)),
  { noreply, State };

% Explicitly stop the generic server.
handle_cast(stop, State) ->
  { stop, normal, State };

% No other asynchronous calls to be handled.
handle_cast(_Message, State) ->
  { noreply, State }.

% No other messages need to be handled.
handle_info(_Info, State) ->
  { noreply, State }.

% Close port and unload driver if server is shutdown.
terminate(_Reason, #state{ name = Name, port = Port }) ->
  port_close(Port),
  erl_ddll:unload(Name),
  ok.

% If code changes, we can leave the state as it is.
code_change(_Old, State, _Extra) ->
  { ok, State }.

%% ----------------------------------------------------------------------------
%% Public functions
%% ----------------------------------------------------------------------------

% Initialize a new generic driver by loading the specified driver and starting
% the generic server. The port is opened in init/1, the pid is returned.
-spec start_link(Path :: string(), Name :: string())
  -> { ok, pid() } | { error, any() }.
start_link(Path, Name) ->
  case erl_ddll:load(Path, Name) of
    ok ->
      gen_server:start_link(?MODULE, #state{
        name = Name
      }, []);
    { error, Reason } ->
      { error, Reason }
  end.

%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------

% This method waits for the port to send data. Waiting for a port is necessary
% in case the port is expected to return a result, since all calls to the
% linked-in driver are made asynchronously.
-spec wait_result(Port :: port())
  -> term() | { error, any() }.
wait_result(Port) ->
  receive
    { Port, { data, Reply } } ->
      try binary_to_term(Reply) of
        Result -> Result
      catch
        _:badarg ->
          { error, eparse }
      end
  end.
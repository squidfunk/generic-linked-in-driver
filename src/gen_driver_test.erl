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

-module(gen_driver_test).
-author('Martin Donath <md@struct.cc>').

% Public functions.
-export([start_link/0, stop/1, sum/2, ping/1, stats/1]).

%% ----------------------------------------------------------------------------
%% Macros
%% ----------------------------------------------------------------------------

% Path and name of the driver.
-define(DRV_PATH, "../priv").
-define(DRV_NAME, "test").

% Commands to be executed by the driver.
-define(CMD_SUM,   1).
-define(CMD_PING,  2).
-define(CMD_STATS, 3).

%% ----------------------------------------------------------------------------
%% Public functions
%% ----------------------------------------------------------------------------

% Load the specified driver and start the associated generic server. The pid of
% the generic server managing the driver is returned.
-spec start_link()
  -> { ok, pid() } | { error, any() }.
start_link() ->
  gen_driver:start_link(?DRV_PATH, ?DRV_NAME).

% As the generic server for drivers may not be part of a supervision tree, we
% provide a simple method to stop it.
-spec stop(Pid :: pid())
  -> ok.
stop(Pid) ->
  gen_server:cast(Pid, stop).

% Take an arbitrary mixed list of integers and floats and add all numbers,
% returning the resulting sum.
-spec sum(Pid :: pid(), Numbers :: [float(), ...])
  -> { ok, float() } | { error, atom() }.
sum(Pid, Numbers) ->
  gen_server:call(Pid, { port, ?CMD_SUM, Numbers }).

% Send an asynchronous request to the driver which returns no result, but
% increments the internal driver and thread counters.
-spec ping(Pid :: pid())
  -> ok.
ping(Pid) ->
  gen_server:cast(Pid, { port, ?CMD_PING }).

% Return statistics (amount of calls) for the driver and the currently active
% thread. This function does not send any arguments to the driver.
-spec stats(Pid :: pid())
  -> { ok, proplists:proplist() } | { error, atom() }.
stats(Pid) ->
  gen_server:call(Pid, { port, ?CMD_STATS }).
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

-module (basic_test).
-author('Martin Donath <md@struct.cc>').

% Include eunit for testing.
-include_lib("eunit/include/eunit.hrl").

%% ----------------------------------------------------------------------------
%% Tests
%% ----------------------------------------------------------------------------

% Start and stop driver.
setup_and_teardown_test()->
  { ok, Driver } = gen_driver_test:start_link(),
  gen_driver_test:stop(Driver).

% Test the driver for basic functionality.
basic_test() ->
  { ok, Pid } = gen_driver_test:start_link(),
  { ok, Sum } = gen_driver_test:sum(Pid, [1, 2, 97]),
  ?assertEqual(100.0, Sum),
  gen_driver_test:stop(Driver).
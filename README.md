# A Generic Linked-in Driver for Erlang

This is an attempt to provide a lightweight and clean implementation of a
generic [linked-in driver][] for communication between Erlang and C/C++
programs. It was designed from the ground up to support multi-threaded,
non-blocking operations, has the ability to hold state across calls and is
entirely customizable. The rationale behind this generic driver is to provide a
default and clean implementation for communication between those two
languages and to minimize the need to write boilerplate code.

Of course there is the Generic Erlang Port Driver ([GEPD][]), which lets you
generate a driver tailored to your needs by specifying function bindings via C
macros. However, this generic linked-in driver implementation takes a different
approach and provides the programmer with an easy to use interface without the
need to generate any code and without any external dependencies. Additionally,
the name of the driver is set via compiler flag, so that multiple instances of
the same driver can be compiled and identified by different names without the
need for code duplication. This is very handy if you design your program to be
configured during compilation and need different configurations.

There's an example linked-in driver included with the source which demonstrates
the features of this implementation. You can dive into the code right away or
keep reading for a detailed reference of features and a simple how-to.

## Theory of Operation

The driver consists of two parts: the Erlang part and the C part. On the Erlang
side the driver is implemented as a generic server. Thus, running a driver
in a supervision hierarchy is simple and also recommended. Every (!) request
to the server can be synchronous (`call`) or asynchronous (`cast`). Naturally,
synchronous requests wait for the server to respond, and return the result,
while asynchronous requests return immediately with `ok`. In the latter case,
any errors on the C side (except the driver crashing due to a segfault or other
critical error) are not reported. Therefore, asynchronous requests should be
used with caution.

Receiving the binary sent by the Erlang part, the C driver parses the request,
passes it to a worker thread for non-blocking execution and returns
immediately. This approach is mandatory, as the main thread on the C side is
executed *synchronously*, thus blocking the VM scheduler. Linked-in drivers run
in the same address space as the Erlang virtual machine, so [beware][Warning]:

> When a driver is loaded it is executed in the context of the emulator, shares
> the same memory and the same thread. This means that all operations in the
> driver must be non-blocking, and that any crash in the driver will bring the
> whole emulator down. In short: you have to be extremely careful!

Thus, before you continue, you should make sure that you have a thorough
understanding of the C language, or more precisely of the art of memory
management. You should also know how to use a debugger ([gdb][] is awesome).

Enough talk, let's play.

## Installation

The generic linked-in driver can be used with plain Erlang or with any OTP
application. The project itself is compiled using [Rebar][], so in order to
play with the example driver, just `cd` into the root directory and `make` the
project. Then go to the `ebin` directory and invoke the `erl` shell:

``` erlang
{ ok, Pid } = gen_driver_test:start_link(). % => { ok,<0.33.0> }
gen_driver_test:sum(Pid, [1,2,3,4]).        % => { ok, 10.0 }
gen_driver_test:ping(Pid).                  % => ok
gen_driver_test:stats(Pid).                 % => { ok, [{ driver, 2 }, { thread, 2 }] }
```

The `stats/1` function returns the number of calls to the driver and the invoked
worker thread. If you start the virtual machine with `erl`, these numbers will
always be the same. This is because the request is processed synchronously, as
no asynchronous worker threads are present. If you start the virtual machine
with `erl +A 4`, and thus four worker threads, you will begin to
see a difference.

Try it.

If you want to use the generic linked-in driver to create your own specific
driver, you basically have two options: using the linked-in driver as a
**dependency** or as a **copy**.

### As a dependency via Rebar (recommended)

Using Rebar (or another build-tool) is absolutely recommended, as the process
of building a driver is somehow non-trivial. Just open your `rebar.config` and
add the following lines to your dependencies:
   
``` erlang
{ deps, [
  { gen_driver, ".*",
    { git, "git://github.com/squidfunk/generic-linked-in-driver.git", "master" }
  }
}.
```

After fetching your dependencies with `rebar get-deps`, you can start driver
development. Rebar expects your C source to be located in a directory called
`c_src`. If it isn't there already, create it. Then you just have to create
a new file, include the [gen_driver.h][] header file and define the necessary
callbacks. The header can be included with:

``` c
#include "gen_driver.h"
```

During compilation, the folders containing the header files have to be
included with the following flag:

```
-I deps/gen_driver/c_src
```

See the section discussing the build process for more information.

### As a copy

If you don't use a build-tool or want the source to be situated in the same
directory as your specific driver, you can just move [gen_driver.c][] and
[gen_driver.h][] to your `c_src` directory, move the Erlang part consisting of
[gen_driver.erl][] to `src`, and get started. Using this approach, including
the header files and linking them during compilation is easier, as all files
are located in the same directory. However, you will mix up your business logic
and dependencies, which is considered bad practice in general.

## Usage

### Erlang

Creating your own specific linked-in driver using this generic implementation
is quite simple. Take a look at the example driver [gen_driver_test.erl][]. The
driver is started with:

``` erlang
{ ok, Pid } = gen_driver:start_link(Path, Name).
```

A tuple containing the pid is returned, which is the process identifier of the
associated generic server. Actions can then be called by invoking the
generic server functions `call` and `cast`:

``` erlang
gen_server:call(Pid, { port, Cmd[, Params] }). % => { ok, Response }
gen_server:cast(Pid, { port, Cmd[, Params] }). % => ok
```

The `Cmd` parameter must be an integer which is used for identifying the
action to perform on the C side. It is recommended to define those numbers as
macros in Erlang and C. While parameters are optional, any valid Erlang term
can be passed.

In the case of our example linked-in driver defined in [gen_driver_test.erl][],
there are two synchronous (`sum/2`, `stats/1`) and one asychronous action
(`ping/1`). Those functions are just simple wrappers which call the respective
C functions:

``` erlang
% Take an arbitrary mixed list of integers and floats and add all numbers,
% returning the resulting sum.
-spec sum(Pid :: pid(), Numbers :: [float(), ...])
  -> { ok, float() } | { error, atom() }.
sum(Pid, Numbers) ->
  gen_server:call(Pid, { port, ?CMD_SUM, Numbers }).
```

### C

Going to the C side [sic!], six callbacks need to be implemented. A
callback-based architecture enables the definition of custom initializers on a
driver- and thread-level. While the driver initializers are executed during the
start up phase, thread initializers are executed upon the first invocation of a
thread. Taken from [gen_driver.h][], these callbacks are:

``` c
/**
 * Callbacks which manage memory of driver-specific state data.
 */
void *init();
void destroy(void *drv_state);

/**
 * Callbacks which manage memory of thread-specific state data.
 */
void *thread_init();
void thread_destroy(void *trd_state);

/**
 * Callbacks which perform load-balancing and the dispatch of the request.
 */
unsigned int *balance(int cmd, unsigned char syn, unsigned int *key);
void dispatch(gd_req_t *req, gd_res_t *res, void *drv_state, void *trd_state);
```

All callbacks must be defined, even if there's no thread-specific data to hold
or no custom balancing among threads. Thus, a minimal implementation which can
be used as a skeleton when developing a new driver could be:

``` c
#include <stdlib.h>
#include <stdio.h>

#include <erl_driver.h>
#include <ei.h>

#include "gen_driver.h"

/**
 * Callback to initialize the application-relevant state data when opening the
 * port driver and to return a pointer to the newly created driver state.
 */
void *
init() {
  return NULL;
}

/**
 * Upon closing the port, this callback is invoked in order to free all memory
 * allocated to the driver state.
 */
void
destroy(void *drv_state) { }

/**
 * Initialize any thread-specific data. This is called, when first dispatching
 * a request to a thread.
 */
void *
thread_init() {
  return NULL;
}

/**
 * Upon closing the port, this callback is invoked in order to free all memory
 * allocated to thread-specific data.
 */
void
thread_destroy(void *trd_state) { }

/**
 * Load balancing among threads. Balancing is implemented as a modulo
 * operation: % THREADS. Return NULL for round-robin strategy. Return the key
 * parameter unchanged to ensure that the same port will always run on the same
 * thread.
 */
unsigned int *
balance(int cmd, unsigned char syn, unsigned int *key) {
  return NULL;
}

/**
 * Dispatch an asynchronous request by invoking the respective callback. If no
 * matching command is found, return an error.
 */
void
dispatch(gd_req_t *req, gd_res_t *res, void *drv_state, void *trd_state) {
  switch (req->cmd) {

    /* Branch here */

    default:
      error_set(res, "command");
  }
}
```

Now let's have a look at the dispatch callback which is the entry point for
requests coming from the Erlang virtual machine. The request as such is
contained in the first parameter, encoded as a binary, and must be explicitly
parsed using the [Erlang Interface][] functions. This is far from being a
complicated task, so for example, parsing a tuple of the form
`{ question, "Why are we here?" }` is as simple as:

``` c
ei_decode_tuple_header(req->buf, &req->index, &size);
ei_decode_atom(req->buf, &req->index, "question");
ei_decode_string(req->buf, &req->index, &question);
```

However, we should check for the return values of the Erlang Interface
functions, as they return with a status `> 0` when an error occurred. Please
see `handle_sum/4` in [gen_driver_test.c][] for a more complex example. Then,
after translating an Erlang term into native C datatypes, and doing our
calculations, we can take the opposite direction and encode the result using
the same interface:

``` c
ei_encode_tuple_header(res->buf, &res->index, 2);
ei_encode_atom(res->buf, &res->index, "answer");
ei_encode_string(res->buf, &res->index, "Plastic!\0");
```

While parsing the request involves the first parameter `req`, the result is
encoded into the second parameter `res`. Of course. The result buffer is
allocated 64 bytes, so after writing some bytes one has to check if the
allocated memory is still sufficient with `res->index < 64`. If it isn't, just
re-allocate some more bytes.

If an error is encountered during parsing or processing, the helper function
`error_set/2` can be used to exit from a handler. The first parameter has to be the
respective `res` struct, the second the atom to return, so e.g.:

``` c
return error_set(res, "badtype"); // => { error, badtype }
```

This translates to the tuple `{ error, badtype }` on the Erlang side. You don't
have to clean the buffer, just set the error and return. However, when you
return, you must make sure that all resources that were allocated are freed.
Otherwise your driver will leak memory, and finally, crash your Erlang
application.

Also remember that asynchronous calls are not able to return anything to the
Erlang side. Even if you write something to the result buffer, the contents
are discarded upon termination. This may be desired behaviour, as every
function on the C side can be called in both ways. However, it just doesn't
make any sense when you're fetching entries from a database.

And that is basically it. Now that you learned the basics, you should take a
closer look at [gen_driver_test.c][], in order to understand the way how the
generic driver is used. Interfacing with the generic linked-in driver, the only
things that need to be done are initializing resources that need to be
persistent across calls and bound to the driver or single threads, decoding the
request and encoding the result. Oh, and your custom business logic of course.

Everything else is already implemented.

## Building

Now that we're done implementing our driver, we need to build it. As already
stated, it is absolutely recommended to use [Rebar][] or a similar build-tool
for this task. Building our newly created linked-in driver, we need to specify
the driver path (e.g. `priv`) and name (e.g. `test`) which must match the file
name of the resulting binary (`test.so`), and link against the generic driver:

``` erlang
{ port_specs, [
  { ".*", "priv/test.so", [
    "deps/gen_driver/c_src/gen_driver.c", "c_src/*.c*"
  ], [
    { env, [
      { "CFLAGS", "$CFLAGS -std=c99 -I deps/gen_driver/c_src -D DRIVER_NAME=test" }
    ] }
  ] }
] }.
{ post_hooks, [
  { compile, "`which rm` -f deps/gen_driver/c_src/*.o" }
] }.
```

The generic driver is implemented using the C99 standard, so in case GCC is
used for compilation, the respective flag needs to be set, since GCC defaults to
C89. The path where the generic driver source is located must also be included. As
mentioned before, multiple instances of the same driver with different names and
configurations can be compiled without any code duplication.

## License

Copyright (c) 2012-2014 Martin Donath

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to
deal in the Software without restriction, including without limitation the
rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
sell copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
IN THE SOFTWARE.

[linked-in driver]: http://www.erlang.org/doc/tutorial/c_portdriver.html
[GEPD]: https://github.com/okeuday/GEPD
[Warning]: http://erlang.org/doc/apps/erts/driver.html#id81992
[gdb]: http://www.gnu.org/software/gdb/
[Rebar]: https://github.com/basho/rebar
[gen_driver.c]: https://github.com/squidfunk/generic-linked-in-driver/blob/master/c_src/gen_driver.c
[gen_driver.h]: https://github.com/squidfunk/generic-linked-in-driver/blob/master/c_src/gen_driver.h
[gen_driver.erl]: https://github.com/squidfunk/generic-linked-in-driver/blob/master/src/gen_driver.erl
[gen_driver_test.c]: https://github.com/squidfunk/generic-linked-in-driver/blob/master/c_src/gen_driver_test.c
[gen_driver_test.erl]: https://github.com/squidfunk/generic-linked-in-driver/blob/master/src/gen_driver_test.erl
[Erlang Interface]: http://erlang.org/doc/man/ei.html

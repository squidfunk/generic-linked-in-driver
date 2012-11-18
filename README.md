# Generic Linked-in Driver

This is an attempt to provide a lightweight and clean implementation of a
generic [linked-in driver][] for communication between Erlang and C/C++
programs. It was designed from the ground up to support multi-threaded,
non-blocking operations, has the ability to hold state across calls and is
entirely customizable. The rationale behind this generic driver is to provide a
well tested and clean implementation for communication between those two
languages and to minimize the need to write boilerplate code.

Of course, there is the Generic Erlang Port Driver ([GEPD][]) which lets you
generate a driver tailored to your needs by specifying function bindings via C
macros. However, this generic linked-in driver takes a different approach and
provides the programmer with an easy to use interface without the need to
generate any code. It has no external dependencies, and even more importantly,
the name of the driver is set via compiler flag, so that multiple instances of
the same driver can be compiled and identified by different names without the
need for code duplication. This is very handy, if you design your program to be
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

## Installation

The generic linked-in driver can be used with plain Erlang or with any OTP
application. The project itself is compiled using [Rebar][], so in order to
play with the example driver, just `cd` into the root directory and `make` the
project. Then go to the `ebin` directory and invoke the `erl` shell:

``` erlang
{ ok, Pid } = gen_driver_test:start_link(). % => { ok,<0.33.0> }
gen_driver_test:sum(Pid, [1,2,3,4]).        % => { ok, 10 }
gen_driver_test:ping(Pid).                  % => ok
gen_driver_test:stats(Pid).                 % => { ok,[{ driver, 2 }, { thread, 2 }] }
```

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

After fetching your dependencies with `rebar get-deps`, you can start using the
driver straight away. Rebar expects your C source to be located in a directory
called `c_src`. If it isn't there already, create it. Then you just have to
create a new file, for example `gen_driver_test.c`, include the `gen_driver.h`
header file and define the necessary callbacks. As the driver is located in
`deps`, you have to include the header files with:

``` C
\#include "../../deps/gen_driver/c_src/gen_driver.h"
```

Sure, this is not very beautiful, but it lets you manage the driver as a
dependency.

### As a copy

If you don't use a build-tool or want the source to be situated in the same
directory as your specific driver, you can just move `gen_driver.c` and
`gen_driver.h` to your `c_src` directory, move the Erlang part contained in
`gen_driver.erl` to `src`, and get started. Using this approach, including
the source files and linking them during compilation is easier, as all files
are located in the same directory. However, you will mix up your business logic
and your dependencies, which is considered bad practice in general.

## Usage

Creating your own specific linked-in driver using this generic implementation
is quite simple. Take a look at the example driver `gen_driver_test.erl`.

``` erlang



```

## Example



## License

Copyright (c) 2012 Martin Donath

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
[Rebar]: https://github.com/basho/rebar
[Warning]: http://erlang.org/doc/apps/erts/driver.html#id81992
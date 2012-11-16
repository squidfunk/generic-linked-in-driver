/*
 * Copyright (c) 2012 Martin Donath <md@struct.cc>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

#ifndef __GEN_DRIVER__
#define __GEN_DRIVER__

#include <erl_driver.h>

/* ----------------------------------------------------------------------------
 * Type definitions
 * ------------------------------------------------------------------------- */

/**
 * This structure holds the thread-specific state data returned by the thread
 * initializer for the respective identifier.
 */
typedef struct gd_trd_ {
  long tid;
  void *state;
} gd_trd_t;

/**
 * This structure holds state across calls. It contains a reference to the port
 * handle, as well as thread- and application-specific state data.
 */
typedef struct gd_ {
  ErlDrvPort port;
  gd_trd_t *trd;
  void *state;
} gd_t;

/**
 * This structure encapsulates the request passed to the control callback,
 * including its buffer, the command to execute and whether its synchronous.
 */
typedef struct gd_req_ {
  char *buf;
  ErlDrvSizeT len;
  int index;
  int cmd;
  unsigned char syn;
} gd_req_t;

/**
 * This structure holds the result buffer of a request, the current buffer
 * length and offset, as well as any potential error.
 */
typedef struct gd_res_ {
  char *buf;
  ErlDrvSizeT len;
  int index;
  char error[32];
} gd_res_t;

/**
 * The purpose of this structure is to hold pointers to the current request,
 * its result and the state of the driver and threads.
 */
typedef struct gd_ptr_ {
  gd_req_t *req;
  gd_res_t *res;
  gd_trd_t *trd_state;
  void *drv_state;
} gd_ptr_t;

/* ----------------------------------------------------------------------------
 * Generic entry points
 * ------------------------------------------------------------------------- */

/**
 * Generic entry points for the driver which are called by the Erlang virtual
 * machine when the driver is accessed from or shall report to Erlang.
 */
ErlDrvData start(ErlDrvPort port, char *cmd);
void stop(ErlDrvData drv_data);
void ready(ErlDrvData drv_data, ErlDrvThreadData thread_data);
ErlDrvSSizeT control(ErlDrvData drv_data, unsigned int cmd, char *buf,
                     ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen);

/* ----------------------------------------------------------------------------
 * Helper functions
 * ------------------------------------------------------------------------- */

/**
 * Helper functions to ease communication.
 */
void error(gd_res_t *res, char *error);

/* ----------------------------------------------------------------------------
 * Driver callbacks
 * ------------------------------------------------------------------------- */

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

/* ----------------------------------------------------------------------------
 * Macros
 * ------------------------------------------------------------------------- */

/**
 * If the name of the driver was not passed to the compiler, abort.
 */
#if !defined(DRIVER_NAME)
  #error DRIVER_NAME not set.
#endif

/**
 * Helper to return the provided identifier for the driver as a string.
 */
#define DRIVER_STRING(name) #name

/**
 * Driver specification defining entry points for the Erlang driver that are
 * called by the Erlang virtual machine when the driver is accessed. Detailed
 * documentation can be found under the following URLs:
 * 
 * [1] http://erlang.org/doc/man/driver_entry.html
 * [2] http://erlang.org/doc/apps/erts/driver.html
 */
#define DRIVER(name) static ErlDrvEntry driver_entry = {                      \
  NULL,		 /* [init] System startup */                                        \
  start, 	 /* [start] open_port/2 is invoked, must return -1 on fail */       \
  stop, 	 /* [stop] Port is closed */                                        \
  NULL, 	 /* [output] There's output from Erlang to the port */              \
  NULL, 	 /* [ready_input] There's input from one of the driver's handles */ \
  NULL, 	 /* [ready_output] Output to one of the handles is possible */      \
  DRIVER_STRING(name),                                                        \
  NULL, 	 /* [finish] The driver is about to be unloaded */                  \
  NULL,		 /* [reserved] */                                                   \
  control, /* [control] I/O control for drivers, invoked by port_control/3 */ \
  NULL, 	 /* [timeout] Handling of timeouts in the driver */                 \
  NULL, 	 /* [outputv] There's output from Erlang to the port (faster!) */   \
  ready,   /* [ready_async] An asynchronous request was completed */          \
  NULL, 	 /* [flush] The port is about to be closed. */                      \
  NULL,		 /* [call] I/O control for drivers, but uses ext. term format */    \
  NULL,		 /* [reserved] */                                                   \
  ERL_DRV_EXTENDED_MARKER,          /* Marker to signal extended driver */    \
  ERL_DRV_EXTENDED_MAJOR_VERSION,   /* Mandatory after extended marker */     \
  ERL_DRV_EXTENDED_MINOR_VERSION,   /* Mandatory after extended marker */     \
  ERL_DRV_FLAG_USE_PORT_LOCKING,    /* Use port-level locking */              \
  NULL,		 /* [reserved] */                                                   \
  NULL, 	 /* [process_exit] A monitored process exits */                     \
  NULL 		 /* [stop_select] Asynchronous event handling */                    \
};                                                                            \
DRIVER_INIT(name) {                                                           \
  return &driver_entry;                                                       \
}

/**
 * Error atoms to be returned by the generic driver.
 */
#define GD_ERR_MEM "memory"
#define GD_ERR_DEC "decode"
#define GD_ERR_BUF "buffer"

#endif
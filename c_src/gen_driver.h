/*
 * Copyright (c) 2012-2014 Martin Donath <md@struct.cc>
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

#ifndef GEN_DRIVER_H
#define GEN_DRIVER_H

#include <erl_driver.h>

/* ----------------------------------------------------------------------------
 * Type definitions
 * ------------------------------------------------------------------------- */

typedef struct gd_trd_t {
  long tid;                            /*!< Thread identifier */
  void *state;                         /*!< Thread state */
} gd_trd_t;

typedef struct gd_t {
  ErlDrvPort port;                     /*!< Erlang port driver */
  gd_trd_t *trd;                       /*!< Threads */
  void *state;                         /*!< State */
} gd_t;

/* ------------------------------------------------------------------------- */

typedef struct gd_req_t {
  char *buf;                           /*!< Buffer */
  ErlDrvSizeT len;                     /*!< Buffer size */
  int index;                           /*!< Buffer offset */
  int cmd;                             /*!< Command */
  unsigned char syn;                   /*!< Synchronous flag */
} gd_req_t;

typedef struct gd_res_t {
  char *buf;                           /*!< Result buffer */
  ErlDrvSizeT len;                     /*!< Result buffer size */
  int index;                           /*!< Result buffer offset */
  char error[64];                      /*!< Error */
} gd_res_t;

typedef struct gd_ptr_t {
  gd_req_t *req;                       /*!< Request */
  gd_res_t *res;                       /*!< Result */
  gd_trd_t *trd_state;                 /*!< Thread state */
  void *drv_state;                     /*!< Driver state */
} gd_ptr_t;

/* ----------------------------------------------------------------------------
 * Generic entry points
 * ------------------------------------------------------------------------- */

extern ErlDrvData
start(
  ErlDrvPort port,                     /* Erlang port driver */
  char *cmd);                          /* Command */

extern void
stop(
  ErlDrvData drv_data);                /* Erlang port driver data */

extern void
ready(
  ErlDrvData drv_data,                 /* Erlang port driver data */
  ErlDrvThreadData thread_data);       /* Erlang port driver thread data */

extern ErlDrvSSizeT
control(
  ErlDrvData drv_data,                 /* Erlang port driver data */
  unsigned int cmd,                    /* Command */
  char *buf,                           /* Buffer */
  ErlDrvSizeT len,                     /* Buffer size */
  char **rbuf,                         /* Result buffer */
  ErlDrvSizeT rlen);                   /* Result buffer size */

/* ----------------------------------------------------------------------------
 * Helper functions
 * ------------------------------------------------------------------------- */

extern void
error_set(
  gd_res_t *res,                       /* Result */
  char *error);                        /* Error */

extern int
error_occurred(
  gd_res_t *res);                      /* Result */

/* ----------------------------------------------------------------------------
 * Driver callbacks
 * ------------------------------------------------------------------------- */

extern void *
init(void);

extern void
destroy(
  void *drv_state);                    /* Driver state */

extern void *
thread_init(void);

extern void
thread_destroy(
  void *trd_state);                    /* Thread state */

extern unsigned int *
balance(
  int cmd,                             /* Command */
  unsigned char syn,                   /* Synchronous flag */
  unsigned int *key);                  /* Balancing key */

extern void
dispatch(
  gd_req_t *req,                       /* Request */
  gd_res_t *res,                       /* Result */
  void *drv_state,                     /* Driver state */
  void *trd_state);                    /* Thread state */

/* ----------------------------------------------------------------------------
 * Macros
 * ------------------------------------------------------------------------- */

/*
 * If the name of the driver was not passed to the compiler, abort.
 */
#if !defined(DRIVER_NAME)
  #error DRIVER_NAME not set.
#endif

/*
 * Helper to return the provided identifier for the driver as a string.
 */
#define DRIVER_STRING(name) #name

/*
 * Driver specification defining entry points for the Erlang driver that are
 * called by the Erlang virtual machine when the driver is accessed. Detailed
 * documentation can be found under the following URLs:
 * 
 * [1] http://erlang.org/doc/man/driver_entry.html
 * [2] http://erlang.org/doc/apps/erts/driver.html
 */
#define DRIVER(name) static ErlDrvEntry driver_entry = {                      \
  NULL,    /* [init] System startup */                                        \
  start,   /* [start] open_port/2 is invoked, must return -1 on fail */       \
  stop,    /* [stop] Port is closed */                                        \
  NULL,    /* [output] There's output from Erlang to the port */              \
  NULL,    /* [ready_input] There's input from one of the driver's handles */ \
  NULL,    /* [ready_output] Output to one of the handles is possible */      \
  DRIVER_STRING(name),                                                        \
  NULL,    /* [finish] The driver is about to be unloaded */                  \
  NULL,    /* [reserved] */                                                   \
  control, /* [control] I/O control for drivers, invoked by port_control/3 */ \
  NULL,    /* [timeout] Handling of timeouts in the driver */                 \
  NULL,    /* [outputv] There's output from Erlang to the port (faster!) */   \
  ready,   /* [ready_async] An asynchronous request was completed */          \
  NULL,    /* [flush] The port is about to be closed. */                      \
  NULL,    /* [call] I/O control for drivers, but uses ext. term format */    \
  NULL,    /* [reserved] */                                                   \
  ERL_DRV_EXTENDED_MARKER,          /* Marker to signal extended driver */    \
  ERL_DRV_EXTENDED_MAJOR_VERSION,   /* Mandatory after extended marker */     \
  ERL_DRV_EXTENDED_MINOR_VERSION,   /* Mandatory after extended marker */     \
  ERL_DRV_FLAG_USE_PORT_LOCKING,    /* Use port-level locking */              \
  NULL,    /* [reserved] */                                                   \
  NULL,    /* [process_exit] A monitored process exits */                     \
  NULL     /* [stop_select] Asynchronous event handling */                    \
};                                                                            \
DRIVER_INIT(name) {                                                           \
  return &driver_entry;                                                       \
}

/* ------------------------------------------------------------------------- */

#define GD_CMD_INIT (1 << 30) - 1      /*!< Initialization of threads */

/* ------------------------------------------------------------------------- */

#define GD_ERR_MEMORY "memory"         /*!< Out of memory */
#define GD_ERR_DECODE "decode"         /*!< Wrong format */

#endif /* GEN_DRIVER_H */
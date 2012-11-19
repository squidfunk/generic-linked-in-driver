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

#include <stdlib.h>
#include <stdio.h>

#include <erl_driver.h>
#include <ei.h>

#include "gen_driver.h"
#include "gen_driver_test.h"

/* ----------------------------------------------------------------------------
 * Internal dispatch callbacks
 * ------------------------------------------------------------------------- */

/**
 * Take an arbitrary mixed list of integers and floats and add all numbers,
 * returning the resulting sum.
 */
static void
handle_sum(gd_req_t *req, gd_res_t *res, gdt_drv_t *drv, gdt_trd_t *trd) {
  int type, size;

  /* Determine type and size */
  if (ei_get_type(req->buf, &req->index, &type, &size) || size <= 0)
    return error(res, GDE_ERR_TPE);

  /* Allocate memory for numbers */
  double *values, sum;
  if ((values = driver_alloc(sizeof(double) * size)) == NULL)
    return error(res, GDE_ERR_MEM);

  /* Decode list */
  switch (type) {

    /* Decode integer list interpreted as string */
    case ERL_STRING_EXT: {
      char value[size];
      if (ei_decode_string(req->buf, &req->index, (char *)&value))
        return error(res, GDE_ERR_DEC);
      for (int v = 0; v < size; v++)
        values[v] = (double)value[v];
      break;
    }

    /* Decode ordinary integer/double list */
    case ERL_LIST_EXT: {
      if (ei_decode_list_header(req->buf, &req->index, &size))
        return error(res, GDE_ERR_DEC);
      for (int v = 0, temp; v < size; v++) {
        ei_get_type(req->buf, &req->index, &type, &temp);
        switch (type) {

          /* Decode integer */
          case ERL_SMALL_INTEGER_EXT:
          case ERL_INTEGER_EXT: {
            long value;
            if (ei_decode_long(req->buf, &req->index, &value))
              return error(res, GDE_ERR_DEC);
            values[v] = (double)value;
            break;
          }

          /* Decode double */
          case ERL_FLOAT_EXT: {
            double value;
            if (ei_decode_double(req->buf, &req->index, &value))
              return error(res, GDE_ERR_DEC);
            values[v] = (double)value;
            break;
          }
          
          /* Unsupported type */
          default:
            return error(res, GDE_ERR_TPE);
        }
      }

      /* A list always contains an empty list at the end */
      if (ei_decode_list_header(req->buf, &req->index, NULL))
        return error(res, GDE_ERR_DEC);
      break;
    }

    /* Unsupported type */
    default:
      return error(res, GDE_ERR_TPE);
  }

  /* Sum up values */
  for (int v = 0; v < size; v++)
    sum += values[v];

  /* Free allocated memory */
  driver_free(values);

  /* Encode resulting sum and return tuple */
  ei_encode_tuple_header(res->buf, &res->index, 2);
  ei_encode_atom(res->buf, &res->index, "ok");
  ei_encode_double(res->buf, &res->index, sum);

  /* Update counters */
  drv->count++;
  trd->count++;
}

/**
 * Send an asynchronous request to the driver which returns no result, but
 * increments the internal driver and thread counters.
 */
static void
handle_ping(gd_req_t *req, gd_res_t *res, gdt_drv_t *drv, gdt_trd_t *trd) {
  drv->count++;
  trd->count++;
}

/**
 * Return statistics (amount of calls) for the driver and the currently active
 * thread. This function does not send any arguments to the driver.
 */
static void
handle_stats(gd_req_t *req, gd_res_t *res, gdt_drv_t *drv, gdt_trd_t *trd) {

  /* Encode return tuple header */
  ei_encode_tuple_header(res->buf, &res->index, 2);
  ei_encode_atom(res->buf, &res->index, "ok");

  /* Encode proplist header */
  ei_encode_list_header(res->buf, &res->index, 2);

  /* Encode driver calls */
  ei_encode_tuple_header(res->buf, &res->index, 2);
  ei_encode_atom(res->buf, &res->index, "driver");
  ei_encode_long(res->buf, &res->index, drv->count++);

  /* Encode thread calls */
  ei_encode_tuple_header(res->buf, &res->index, 2);
  ei_encode_atom(res->buf, &res->index, "thread");
  ei_encode_long(res->buf, &res->index, trd->count++);

  /* Encode empty list at the end */
  ei_encode_empty_list(res->buf, &res->index);
}

/* ----------------------------------------------------------------------------
 * Driver callbacks
 * ------------------------------------------------------------------------- */

/**
 * Callback to initialize the application-relevant state data when opening the
 * port driver and to return a pointer to the newly created driver state.
 */
void *
init() {
  gdt_drv_t *drv;
  if ((drv = driver_alloc(sizeof(gdt_drv_t))) == NULL) /* destroy */
    return NULL;
  drv->count = 0;
  return (void *)drv;
}

/**
 * Upon closing the port, this callback is invoked in order to free all memory
 * allocated to the driver state.
 */
void
destroy(void *drv_state) {
  driver_free(drv_state); /* init */
}

/**
 * Initialize any thread-specific data. This is called, when first dispatching
 * a request to a thread.
 */
void *
thread_init() {
  gdt_trd_t *trd;
  if ((trd = driver_alloc(sizeof(gdt_trd_t))) == NULL) /* thread_destroy */
    return NULL;
  trd->count = 0;
  return (void *)trd;
}

/**
 * Upon closing the port, this callback is invoked in order to free all memory
 * allocated to thread-specific data.
 */
void
thread_destroy(void *trd_state) {
  driver_free(trd_state); /* thread_init */
}

/**
 * Load balancing among threads. Balancing is implemented as a modulo
 * operation: % THREADS. Return null for round-robin strategy.
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
  gdt_drv_t *drv = drv_state;
  gdt_trd_t *trd = trd_state;

  /* Dispatch the request */
  switch (req->cmd) {
    case GDE_CMD_SUM:
      handle_sum(req, res, drv, trd);
      break;
    case GDE_CMD_PNG:
      handle_ping(req, res, drv, trd);
      break;
    case GDE_CMD_STS:
      handle_stats(req, res, drv, trd);
      break;
    default:
      error(res, GDE_ERR_CMD);
  }
}
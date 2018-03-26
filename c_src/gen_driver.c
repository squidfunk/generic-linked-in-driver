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

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include <erl_driver.h>
#include <ei.h>

#include "gen_driver.h"

/* ----------------------------------------------------------------------------
 * Declarations
 * ------------------------------------------------------------------------- */

static int threads;                    /*!< Number of threads */

/* ----------------------------------------------------------------------------
 * Internal functions
 * ------------------------------------------------------------------------- */

/*!
 * Encode an Erlang term of the form "ok".
 *
 * \param[in,out] rbuf  Result buffer
 * \param[in,out] index Result buffer offset
 */
static void
encode_ok(char *rbuf, int *index) {
  assert(rbuf && index);
  ei_encode_atom(rbuf, index, "ok");
}

/*!
 * Encode an Erlang term of the form "{ error, Error }".
 *
 * \param[in,out] rbuf  Result buffer
 * \param[in,out] index Result buffer offset
 * \param[in]     error Error atom
 */
static void
encode_error(char *rbuf, int *index, const char *error) {
  assert(rbuf && index && error);
  ei_encode_tuple_header(rbuf, index, 2);
  ei_encode_atom(rbuf, index, "error");
  ei_encode_atom(rbuf, index, error);
}

/*!
 * Internal helper function to ease dispatching by directly passing the
 * request, result and driver and thread state to the dispatch callback.
 *
 * \param[in,out] data Driver data
 */
static void
async(void *data) {
  assert(data);
  gd_ptr_t *ptr = data;
  gd_trd_t *trd = ptr->trd_state;

  /* Determine thread identifier */
  long tid = (long)erl_drv_thread_self();

  /* Check for thread-specific state */
  void *trd_state = NULL;
  for (int t = 0; t < threads; t++) {
    if (trd[t].tid == 0 && (trd[t].tid = tid))
      trd[t].state = thread_init();

    /* Select thread-specific state */
    if (trd[t].tid == tid && (trd_state = trd[t].state))
      break;
  }

  /* Dispatch unless an error or initialization occurred */
  if (ptr->req->cmd == GD_CMD_INIT) {
    ei_decode_list_header(ptr->req->buf, &ptr->req->index, NULL);
  } else if (!strlen(ptr->res->error))
    dispatch(ptr->req, ptr->res, ptr->drv_state, trd_state);
}

/* ----------------------------------------------------------------------------
 * Generic entry points
 * ------------------------------------------------------------------------- */

/*!
 * The port is opened, so try to allocate memory for the driver to hold the
 * port descriptor and initialize any state-relevant data via callback.
 *
 * \param[in] port Erlang port driver
 * \param[in] cmd  Command
 * \return         Erlang port driver data
 */
extern ErlDrvData
start(ErlDrvPort port, char *cmd) {
  assert(port && cmd);
  gd_t *drv; ErlDrvSysInfo info;

  /* Set communication to binary mode */
  set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

  /* Allocate memory for driver and state */
  if (!(drv = driver_alloc(sizeof(gd_t)))) /* stop */
    driver_failure_atom(port, GD_ERR_MEMORY);
  else if (!(drv->state = init()))
    driver_failure_atom(port, GD_ERR_MEMORY);
  else
    drv->port = port;

  /* Determine number of async threads */
  driver_system_info(&info, sizeof(ErlDrvSysInfo));
  threads = info.async_threads ? info.async_threads : 1;

  /* Allocate memory for thread states */
  if (!(drv->trd = driver_alloc(sizeof(gd_trd_t) * threads))) /* stop */
    driver_failure_atom(port, GD_ERR_MEMORY);
  for (int t = 0; t < threads; t++)
    drv->trd[t].tid = 0;
  return (ErlDrvData)drv;
}

/*!
 * The port is about to be closed, so destroy the driver and thread states via
 * callback and free all allocated memory.
 *
 * \param[in] drv_data Erlang port driver data
 */
extern void
stop(ErlDrvData drv_data) {
  assert(drv_data);
  gd_t *drv = (void *)drv_data;

  /* Destroy thread and driver states */
  destroy(drv->state);
  for (int t = 0; t < threads; t++)
    if (drv->trd[t].tid)
      thread_destroy(drv->trd[t].state);

  /* Free thread states and driver */
  driver_free(drv->trd); /* start */
  driver_free(drv); /* start */
}

/*!
 * An asynchronous request was completed, so output the resulting binary to the
 * Erlang virtual machine and free all resources allocated for dispatching.
 *
 * If the caller made a synchronous request and is interested in the result,
 * output the data contained in the result buffer to the port, unless an error
 * occurred. In this case just return the error. If no data was set (the index
 * is still at position 1), just return ok.
 *
 * \param[in] drv_data    Erlang port driver data
 * \param[in] thread_data Erlang port driver thread data
 */
extern void
ready(ErlDrvData drv_data, ErlDrvThreadData thread_data) {
  assert(drv_data && thread_data);
  gd_t     *drv = (void *)drv_data;
  gd_ptr_t *ptr = (void *)thread_data;

  /* Check, if we reached the end of the request buffer */
  if (!error_occurred(ptr->res) && ptr->req->len != ptr->req->index)
    error_set(ptr->res, GD_ERR_DECODE);

  /* Check for error on synchronous request, output data */
  if (ptr->req->syn) {
    if (error_occurred(ptr->res) && (ptr->res->index = 1))
      encode_error(ptr->res->buf, &ptr->res->index, ptr->res->error);
    else if (ptr->res->index == 1)
      encode_ok(ptr->res->buf, &ptr->res->index);
    driver_output(drv->port, ptr->res->buf, ptr->res->index);
  }

  /* Free request and result */
  driver_free(ptr->req->buf); /* control */
  driver_free(ptr->res->buf); /* control */
  driver_free(ptr->req); /* control */
  driver_free(ptr->res); /* control */
  driver_free(ptr); /* control */
}

/*!
 * This function handles the actual request from the Erlang virtual machine and
 * is invoked when port_control/3 is called.
 *
 * It is important to denote that this function must return as immediate as
 * possible, as all schedulers are blocked while executing this code. Thus,
 * the request is processed asynchronously after allocating memory in order to
 * pass the request to a worker thread.
 *
 * We need to explicitly allocate memory for the request and result, as we
 * dispatch the request to one of several worker threads and all buffers are
 * freed at the end of this function. Next, we allocate memory for both,
 * request and result buffers, and copy the request buffer containing the
 * actual request as a binary via memcpy to make it accessible by worker
 * threads for processing.
 *
 * We also need to know, whether the Erlang process calling the driver is
 * interested in the result, and thus made a synchronous request. If not, the
 * 30th bit ist set to 1. However, the result must still be initialized. Then
 * we start at the offset 0, strip the version byte from the buffer and set the
 * index to the beginning of the actual content. If no error occurred while
 * stripping the version byte, the binary data we received seems to be non-
 * corrupted, so we can initialize the result buffer, which we, for now,
 * allocate 64 bytes, encode the version byte and deploy the request.
 *
 * \param[in]     drv_data Erlang port driver data
 * \param[in]     cmd      Command
 * \param[in]     buf      Buffer
 * \param[in]     len      Buffer size
 * \param[in,out] rbuf     Result buffer
 * \param[in]     rlen     Result buffer size
 * \return                 Exit code
 */
extern ErlDrvSSizeT
control(
    ErlDrvData drv_data, unsigned int cmd, char *buf,
    ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen) {
  assert(drv_data && buf && len && rbuf && rlen);
  int index = 0, version;
  ei_encode_version(*rbuf, &index);

  /* Allocate memory for request and result */
  gd_ptr_t *ptr; gd_req_t *req; gd_res_t *res;
  if (!(ptr = driver_alloc(sizeof(gd_ptr_t))) || /* ready */
      !(req = driver_alloc(sizeof(gd_req_t))) || /* ready */
      !(res = driver_alloc(sizeof(gd_res_t))))   /* ready */
    return encode_error(*rbuf, &index, GD_ERR_MEMORY), index;

  /* Allocate memory for buffers */
  if (!(req->buf = driver_alloc(sizeof(char) * len)) || /* ready */
      !(res->buf = driver_alloc(sizeof(char) * 64)))    /* ready */
    return encode_error(*rbuf, &index, GD_ERR_MEMORY), index;
  memcpy(req->buf, buf, len);

  /* Check for synchronous request and strip version byte */
  req->syn = cmd ^ (1 << 30);
  req->len = len; req->index = 0; req->cmd = cmd & ((1 << 30) - 1);
  if (ei_decode_version(req->buf, &req->index, &version)) {
    encode_error(*rbuf, &index, GD_ERR_DECODE);

  /* Version fine, initialize buffers */
  } else {
    res->len = 64; res->index = 0; res->error[0] = 0;
    ei_encode_version(res->buf, &res->index);

    /* Save reference to state variables */
    ptr->drv_state = ((gd_t *)drv_data)->state;
    ptr->trd_state = ((gd_t *)drv_data)->trd;
    ptr->req = req; ptr->res = res;

    /* Pass the request to a worker thread */
    unsigned int key = driver_async_port_key(((gd_t *)drv_data)->port);
    driver_async(((gd_t *)drv_data)->port,
      balance(req->cmd, req->syn, &key), async, ptr, NULL);
    encode_ok(*rbuf, &index);
  }
  return index;
}

/* ----------------------------------------------------------------------------
 * Helper functions
 * ------------------------------------------------------------------------- */

/*!
 * Set an error to be returned by the driver.
 *
 * \param[in,out] res   Result
 * \param[in]     error Error
 */
extern void
error_set(gd_res_t *res, char *error) {
  assert(res && error);
  strcpy(res->error, error);
  res->error[strlen(error)] = 0;
}

/*!
 * Examine the result and return true if an error occurred.
 *
 * \param[in] res Result
 * \return        Test result
 */
extern int
error_occurred(gd_res_t *res) {
  assert(res);
  return !!strlen(res->error);
}

/* ----------------------------------------------------------------------------
 * Driver initialization
 * ------------------------------------------------------------------------- */

/*
 * This macro expands the glue code which is needed to connect up the C driver
 * and the Erlang virtual machine.
 */
DRIVER(DRIVER_NAME);
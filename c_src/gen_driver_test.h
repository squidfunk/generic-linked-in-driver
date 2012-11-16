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

#ifndef __GEN_DRIVER_EXAMPLE__
#define __GEN_DRIVER_EXAMPLE__

#include "gen_driver.h"

/* ----------------------------------------------------------------------------
 * Type definitions
 * ------------------------------------------------------------------------- */

/**
 * This structure is an example for driver-specific state data and holds the
 * amount of calls made to the driver.
 */
typedef struct gde_drv_t {
  int count;
} gde_drv_t;

/**
 * This structure is an example for thread-specific state data and holds the
 * amount of calls made to the respective thread.
 */
typedef struct gde_trd_ {
  int count;
} gde_trd_t;

/* ----------------------------------------------------------------------------
 * Macros
 * ------------------------------------------------------------------------- */

/**
 * Actions to be executed by the driver.
 */
#define GDE_CMD_SUM 0x01
#define GDE_CMD_PNG 0x02
#define GDE_CMD_STS 0x03

/**
 * Error atoms to be returned by the generic driver.
 */
#define GDE_ERR_MEM "memory"
#define GDE_ERR_DEC "decode"
#define GDE_ERR_CMD "command"
#define GDE_ERR_TPE "type"

#endif
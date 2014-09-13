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

#ifndef GEN_DRIVER_TEST_H
#define GEN_DRIVER_TEST_H

#include "gen_driver.h"

/* ----------------------------------------------------------------------------
 * Type definitions
 * ------------------------------------------------------------------------- */

typedef struct gdt_drv_t {
  int count;                           /*!< Driver call count */
} gdt_drv_t;

typedef struct gdt_trd_t {
  int count;                           /*!< Thread call count */
} gdt_trd_t;

/* ----------------------------------------------------------------------------
 * Macros
 * ------------------------------------------------------------------------- */

#define GDE_CMD_SUM   1                /*!< handle_sum/4 */
#define GDE_CMD_PING  2                /*!< handle_ping/4 */
#define GDE_CMD_STATS 3                /*!< handle_stats/4 */

/* ------------------------------------------------------------------------- */

#define GDE_ERR_MEMORY  "memory"       /*!< Out of memory */
#define GDE_ERR_DECODE  "decode"       /*!< Wrong format */
#define GDE_ERR_COMMAND "command"      /*!< Unsupported command */
#define GDE_ERR_TYPE    "type"         /*!< Unsupported type */

#endif /* GEN_DRIVER_TEST_H */
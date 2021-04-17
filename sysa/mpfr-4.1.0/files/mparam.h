/*
SPDX-FileCopyrightText: 2005-2020 Free Software Foundation, Inc.
SPDX-License-Identifier: GPL-3.0-or-later
*/

/* This file is truncated version of src/mparam.h
*/

#ifndef __MPFR_IMPL_H__
# error "MPFR Internal not included"
#endif

#define MPFR_TUNE_CASE "default"

/****************************************************************
 * Default values of Threshold.                                 *
 * Must be included in any case: it checks, for every constant, *
 * if it has been defined, and it sets it to a default value if *
 * it was not previously defined.                               *
 ****************************************************************/
#include "generic/mparam.h"

/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 *
 * This file is part of GNU Mes.
 *
 * GNU Mes is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 *
 * GNU Mes is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GNU Mes.  If not, see <http://www.gnu.org/licenses/>.
 */
#ifndef __MES_MATH_H
#define __MES_MATH_H 1

#if SYSTEM_LIBC
#undef __MES_MATH_H
#include_next <math.h>
#else // ! SYSTEM_LIBC

double atan2 (double y, double x);
double ceil (double x);
double cos (double x);
double exp (double x);
double fabs (double number);
double floor (double x);
double ldexp (double value, int exponent);
double log (double x);
double modf (double value, double *integer_part);
double pow (double base, double power);
double sin (double x);
double sqrt (double x);

#endif // ! SYSTEM_LIBC

#endif // __MES_MATH_H

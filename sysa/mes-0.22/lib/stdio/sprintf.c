/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <mes/lib.h>
#include <stdarg.h>
#include <stdio.h>

int
sprintf (char *str, char const *format, ...)
{
  va_list ap;
  int r;
#if __GNUC__ && __x86_64__ && !SYSTEM_LIBC
#define __FUNCTION_ARGS 2
  ap += (__FOO_VARARGS + (__FUNCTION_ARGS << 1)) << 3;
#undef __FUNCTION_ARGS
#endif
  va_start (ap, format);
  r = vsprintf (str, format, ap);
  va_end (ap);
  return r;
}

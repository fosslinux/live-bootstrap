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

int
//stdarg1 (long one, ...)
stdarg1 (long *one, ...)
{
  va_list ap;
  char c;
  int r;

  va_start (ap, one);
#if __GNUC__ && __x86_64__ && !SYSTEM_LIBC
#define __FUNCTION_ARGS 1
  ap += (__FOO_VARARGS + (__FUNCTION_ARGS << 1)) << 3;
#undef __FUNCTION_ARGS
#endif
  c = va_arg (ap, char);
  r = c;
  eputs ("c:");
  eputs (itoa (c));
  eputs ("\n");

  va_end (ap);
  return r;
}

int
///stdarg2 (long one, long two, ...)
stdarg2 (long *one, long *two, ...)
{
  va_list ap;
  char c;
  int r;

  va_start (ap, two);
#if __GNUC__ && __x86_64__ && !SYSTEM_LIBC
#define __FUNCTION_ARGS 2
  ap += (__FOO_VARARGS + (__FUNCTION_ARGS << 1)) << 3;
#undef __FUNCTION_ARGS
#endif
  c = va_arg (ap, char);
  r = c;
  eputs ("c:");
  eputs (itoa (c));
  eputs ("\n");

  va_end (ap);
  return r;
}

int
//stdarg3 (long one, long two, long three, ...)
stdarg3 (long *one, long *two, long *three, ...)
{
  va_list ap;
  char c;
  int r;

  va_start (ap, three);
#if __GNUC__ && __x86_64__ && !SYSTEM_LIBC
#define __FUNCTION_ARGS 3
  ap += (__FOO_VARARGS + (__FUNCTION_ARGS << 1)) << 3;
#undef __FUNCTION_ARGS
#endif
  c = va_arg (ap, char);
  r = c;
  eputs ("c:");
  eputs (itoa (c));
  eputs ("\n");

  va_end (ap);
  return r;
}

int
main ()
{
  char c = 'm';
  char buf[20];

  if (stdarg1 (-1, c) != c)
    return 1;

  c = 'w';
  if (stdarg2 (-1, -2, c) != c)
    return 2;

  c = 'g';
  if (stdarg3 (-1, -2, -3, c) != c)
    return 3;

  return 0;
}

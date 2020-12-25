/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
#include <stdlib.h>

#if SYSTEM_LIBC
#include <fcntl.h>
#include <stdarg.h>
// The Mes C Library defines and initializes these in crt1
int __stdin = STDIN;
int __stdout = STDOUT;
int __stderr = STDERR;

int
mes_open (char const *file_name, int flags, int mask)
{
  __ungetc_init ();
  int filedes = open (file_name, flags, mask);
  if (filedes > 2)
    __ungetc_clear (filedes);
  return filedes;
}

#else // !SYSTEM_LIBC

int
mes_open (char const *file_name, int flags, int mask)
{
  return _open3 (file_name, flags, mask);
}

#endif // !SYSTEM_LIBC

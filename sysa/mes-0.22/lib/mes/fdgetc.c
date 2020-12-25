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
#include <limits.h>
#include <string.h>
#include <sys/resource.h>
#include <unistd.h>

int __ungetc_buf[__FILEDES_MAX + 1] = { 0 };

int
__ungetc_p (int filedes)
{
  return __ungetc_buf[filedes] >= 0;
}

void
__ungetc_init ()
{
  if (__ungetc_buf[__FILEDES_MAX] == 0)
    memset (__ungetc_buf, -1, (__FILEDES_MAX + 1) * sizeof (int));
}

void
__ungetc_clear (int filedes)
{
  __ungetc_buf[filedes] = -1;
}

void
__ungetc_set (int filedes, int c)
{
  __ungetc_buf[filedes] = c;
}

int
fdgetc (int fd)
{
  __ungetc_init ();

  char c;
  int i = __ungetc_buf[fd];
  if (i >= 0)
    __ungetc_buf[fd] = -1;
  else
    {
      int r = read (fd, &c, 1);
      if (r < 1)
        return -1;
      i = (int) c;
    }
  if (i < 0)
    i += 256;

  return i;
}

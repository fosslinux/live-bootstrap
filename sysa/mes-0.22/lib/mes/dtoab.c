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

char *
dtoab (double d, int base, int signed_p)
{
  static char dtoa_buf[40];
  long i = (long) d;
  char *p = ntoab (i, base, signed_p);
  strcpy (dtoa_buf, p);
  long f = (d - (double) i) * (double) 100000000;
  if (f)
    {
      if (f < 0)
        f = -f;
      strcat (dtoa_buf, ".");
      p = ntoab (f, base, 1);
      strcat (dtoa_buf, p);
      p = strchr (dtoa_buf, 0);
      p--;
      while (*p && *p == '0')
        *p-- = 0;
    }
  return dtoa_buf;
}

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
#include <fcntl.h>
#include <stdarg.h>

int
_open2 (char const *file_name, int flags)
{
  int mask = 0777;
  return _open3 (file_name, flags, mask);
}

int
open (char const *file_name, int flags, ...)
{
  if (flags & O_CREAT)
    {
      va_list ap;
      va_start (ap, flags);
      int mask = va_arg (ap, int);
      int r = _open3 (file_name, flags, mask);
      va_end (ap);
      return r;
    }
  else
    return _open2 (file_name, flags);
}

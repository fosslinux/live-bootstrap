/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>

int
access (char const *file_name, int how)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("access stub\n");
  stub = 1;
  errno = 0;
  if (how == R_OK || how == F_OK)
    {
      int filedes = _open3 (file_name, O_RDONLY, 0);
      if (filedes >= 2)
        return 0;
    }
  else if (how == W_OK)
    return 0;
  else if (how == X_OK)
    return 0;
  return -1;
}

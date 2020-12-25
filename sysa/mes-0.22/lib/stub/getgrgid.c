/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
#include <grp.h>

struct group *
getgrgid (gid_t gid)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("getgrid stub\n");
  static char *groups[2] = { "root", 0 };
#if SYSTEM_LIBC
  static struct group root = { "root", 0, 0 };
  root.gr_mem = groups;
#else
  static struct group root = { "root", 0, groups };
#endif
  stub = 1;
  errno = 0;
  return &root;
}

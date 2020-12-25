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

#include <mes/lib.h>

#include <inttypes.h>
#include <string.h>

struct option
{
  char const *name;
  uint8_t index;
  uint16_t flags;
  int barf;
};

int
main ()
{
  struct option h = { "help", 0, 10, 1 };
  struct option o = { "output", 1, 11, 1 };
  struct option v = { "version", 0, 0, 1 };

  if (strcmp (h.name, "help"))
    return 1;
  if (h.index != 0)
    return 2;
  if (h.flags != 10)
    return 3;

  struct option *p = &o;
  if (strcmp (p->name, "output"))
    return 4;
  if (p->index != 1)
    return 5;
  if (p->flags != 11)
    return 6;

  p = &v;
  v.index = 2;
  p->flags = 12;
  if (v.index != 2)
    return 7;
  if (v.flags != 12)
    return 8;

  return 0;
}

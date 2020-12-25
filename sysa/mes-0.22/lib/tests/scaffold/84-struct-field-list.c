/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <string.h>

struct foo
{
  int i;
  int *j;
  struct foo *bar;
  struct foo *baz;
};

struct bar
{
  int i, *j;
  struct bar *bar, *baz;
};

int
main ()
{
  struct foo f = { 0, 0, 0, 0 };
  struct foo g = { 1, 0, 0, 0 };
  f.j = &f.i;
  g.j = &g.i;
  f.bar = &f;
  f.baz = &g;

  struct bar b;
  memcpy (&b, &f, sizeof (struct foo));
  if (b.i != 0)
    return 1;
  if (*b.j != 0)
    return 2;
  if (b.bar->i != 0)
    return 3;
  if (*b.baz->j != 1)
    return 4;
  return 0;
}

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

struct foo
{
  int a;
  int b;
};

struct foo g_foo[2] = { 0, 1, 2, 3 };

struct bar
{
  int bar[4];
};

struct bar g_bar = { 101, 102 };

typedef struct bar bar_struct;
typedef struct bar foo_struct;

int
main ()
{
  if (g_foo[0].a != 0)
    return 1;
  if (g_foo[0].b != 1)
    return 2;
  if (g_foo[1].a != 2)
    return 3;
  if (g_foo[1].b != 3)
    return 4;

  void *p = &g_foo;
  struct foo *pfoo = (((struct foo *) p) + 1);
  if (pfoo->a != 2)
    return 5;
  if (pfoo->b != 3)
    return 6;

  int *pi = &g_foo;
  if (*pi != 0)
    return 7;

  pi = &g_bar;
  if (*pi != 101)
    return 8;

  struct bar bar = { 0x22, 0x33 };
  pi = &bar;
  if (*pi != 0x22)
    return 9;

  bar_struct bs;
  bs.bar[0] = 102;
  pi = &bs;
  if (*pi != 102)
    return 10;

  foo_struct fs;
  fs.bar[0] = 0x22;
  fs.bar[1] = 0x33;

  pi = &fs;
  if (*pi != 0x22)
    return 11;
  pi++;

  if (*pi != 0x33)
    return 12;

  foo_struct *pfs = &fs;
  pfs->bar[3] = 0x44;
  pfs->bar[4] = 0x55;

  pi = &fs.bar[3];
  if (*pi != 0x44)
    return 13;
  pi++;
  if (*pi != 0x55)
    return 14;

  return 0;
}

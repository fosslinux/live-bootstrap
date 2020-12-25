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
#include <stdio.h>

union u
{
  int bar;
  int baz;
};

struct foo
{
  union u u;
};

struct anon
{
  union
  {
    int bar;
    int baz;
  };
};


int
main ()
{
  struct foo f = { 2 };
  printf ("f.u.bar=%d\n", f.u.bar);
  if (f.u.bar != 2)
    return 1;
  printf ("f.u.baz=%d\n", f.u.baz);
  if (f.u.baz != 2)
    return 1;

  struct anon a = { 2 };
  printf ("a.bar=%d\n", a.bar);
  if (a.bar != 2)
    return 1;
  printf ("a.baz=%d\n", a.baz);
  if (a.baz != 2)
    return 1;

  return 0;
}

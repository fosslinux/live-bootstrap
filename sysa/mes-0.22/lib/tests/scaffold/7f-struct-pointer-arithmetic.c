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

#include <mes/lib.h>
#include <stdio.h>

struct foo;
typedef struct foo foo_struct;

struct foo
{
  //struct foo **foo;
  foo_struct **foo;
};

struct foo g_foo[2];

int
main ()
{
  struct foo foo;
  foo.foo = g_foo;
  void *p;
  void *q;

  p = &foo.foo[0];
  q = foo.foo;
  eputs ("f:");
  eputs (itoa (foo.foo));
  eputs ("\n");
  eputs ("p:");
  eputs (itoa (p));
  eputs ("\n");
  eputs ("q:");
  eputs (itoa (q));
  eputs ("\n");
  if (q != p)
    return 1;

  p = &foo.foo[1];
  q = foo.foo + 1;
  eputs ("f:");
  eputs (itoa (foo.foo));
  eputs ("\n");
  eputs ("p:");
  eputs (itoa (p));
  eputs ("\n");
  eputs ("q:");
  eputs (itoa (q));
  eputs ("\n");
  if (q != p)
    return 2;

  struct foo *pfoo = &foo;
  p = &pfoo->foo[1];
  q = pfoo->foo + 1;
  eputs ("f:");
  eputs (itoa (pfoo->foo));
  eputs ("\n");
  eputs ("p:");
  eputs (itoa (p));
  eputs ("\n");
  eputs ("q:");
  eputs (itoa (q));
  eputs ("\n");
  if (q != p)
    return 3;

  return 0;
}

/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

typedef struct foo
{
  int i;
} foo;

typedef struct
{
  int i;
  struct foo f;
  struct foo *p;
} bar;


bar baz[2] = { 1, 2, 3, 4, 5, 6 };

bar *list[2];

int
main ()
{
  bar one = {0};
  printf ("one.i\n", one.i);
  if (one.i != 0)
    return 1;

  printf ("one.f.i\n", one.f.i);
  if (one.f.i != 0)
    return 2;

  bar b0 = {2};
  struct foo f0 = {0};
  struct foo *pf = &f0;
  list[0] = &b0;
  list[0]->p = pf;

  eputs ("b0.i="); eputs (itoa (b0.i)); eputs ("\n");
  if (b0.i != 2)
    return 3;
  eputs ("b0.p->i="); eputs (itoa (b0.p->i)); eputs ("\n");
  if (b0.p->i != 0)
    return 4;

  return 0;
}

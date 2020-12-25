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

#include <string.h>

#include <mes/lib.h>

struct baz
{
  int i;
  int j;
};

struct foo
{
  int **bar;
};

void
add0 (void *ptab)
{
  void **pp = *(void ***) ptab;
bla:
  pp[0] = 0x11223344;
}

void
add1 (void *ptab)
{
  void ***x = (void ***) ptab;
bla:
  *(void ***) ptab = 0x22334455;
}

void
add2 (void *ptab)
{
  void ***x = (void ***) ptab;
bla:
  *x = 0x33445566;
}

struct foo *hash_ident[10];

int
main ()
{
  int i = 1;
  int *p = &i;
  struct foo f;
  f.bar = &p;
  eputs ("f.bar:");
  eputs (itoa (f.bar));
  eputs ("\n");

  add0 (&f.bar);
  eputs ("f.bar:");
  eputs (itoa (*f.bar));
  eputs ("\n");
  if (*f.bar != 0x11223344)
    return 1;

  add1 (&f.bar);
  eputs ("f.bar:");
  eputs (itoa (f.bar));
  eputs ("\n");
  if (f.bar != 0x22334455)
    return 2;

  add2 (&f.bar);
  eputs ("f.bar:");
  eputs (itoa (f.bar));
  eputs ("\n");
  if (f.bar != 0x33445566)
    return 3;

  hash_ident[0] = 10;
  *hash_ident = 0;
  memset (hash_ident, 0, 10);

  struct baz b;
  b.i = b.j = 1;
  if (b.i != 1)
    return 4;

  struct baz *pb = &b;
  pb->i = pb->j = 1;
  if (pb->i != 1)
    return 5;

  return 0;
}

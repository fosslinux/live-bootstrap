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

//NYACC
//#define offsetof(type, field) ((size_t) &((type *)0)->field)
#if __MESC__
#define offsetof(type, field) (&((type *)0)->field)
#else
#define offsetof(type, field) ((size_t)&((type *)0)->field)
#endif

int
main ()
{
  foo f = { 1 };
  printf ("f.i=%d\n", f.i);

  bar b = { 1, 2, &f };
  printf ("b.i=%d\n", b.i);

  printf ("b.f.i=%d\n", b.f.i);
  if (b.f.i != 2)
    return 1;

  printf ("b.p->i=%d\n", b.p->i);
  if (b.p->i != 1)
    return 2;

  bar *p = &b;
  p->i = 2;
  printf ("p->i=%d\n", b.i);

  p->i++;
  printf ("p->i=%d\n", b.i);

  p->i--;
  printf ("p->i=%d\n", b.i);

  printf ("p->f.i=%d\n", p->f.i);
  if (p->f.i != 2)
    return 3;

  printf ("p->p->i=%d\n", p->p->i);
  if (p->p->i != 1)
    return 4;

  bar **pp = &p;
  (*pp)->i = 3;
  printf ("(*pp)->i=%d\n", b.i);

  printf ("sizeof i:%d\n", sizeof (p->i));
  if ((sizeof p->i) != 4)
    return 5;

  printf ("offsetof g=%d\n", (offsetof (bar, f)));
#if __MESC__
  //if ((offsetof (bar ,f)) != 4) return 6;
  //#define offsetof(type, field) (&((type *)0)->field)
  if ((&((bar *) 0)->f) != 4)
    return 6;

#else
  if ((offsetof (bar, f)) != 4)
    return 6;
#endif

  printf ("(*pp)->b.i=%d\n", (*pp)->f.i);
  if ((*pp)->f.i != 2)
    return 7;

  if (baz[0].i != 1)
    return 8;
  printf ("baz[0].f.i=%d\n", baz[0].f.i);
  if (baz[0].f.i != 2)
    return 9;

  printf ("baz[1].i=%d\n", baz[1].i);
  if (baz[1].i != 4)
    return 10;
  printf ("baz[1].f.i=%d\n", baz[1].f.i);
  if (baz[1].f.i != 5)
    return 11;

  return 0;
}

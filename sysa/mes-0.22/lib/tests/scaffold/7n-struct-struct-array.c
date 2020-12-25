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
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct file
{
  char name[10];
} file_struct;

#define STACK_SIZE 2
struct state
{
  int bla;
  file_struct *stack[STACK_SIZE];
  char buf[100];
  file_struct **stack_ptr;
  char buf1[100];
};

int
main ()
{
  struct state s;
  struct state *ps;
  ps = &s;
  eputs ("0\n");

  s.stack_ptr = s.stack;
  ps->stack_ptr = ps->stack;
  eputs ("ps->stack=");
  eputs (itoa (ps->stack));
  eputs ("\n");

  eputs ("1\n");
  if (ps->stack_ptr >= ps->stack + STACK_SIZE)
    return 1;
  eputs ("2\n");

  struct file f = { "first.h" };
#if 0                           //__MESC__
  strcpy (f.name, "first.h");
#endif
  eputs (f.name);
  eputs ("\n");

  *ps->stack_ptr = &f;

  eputs ("3\n");
  ++ps->stack_ptr;
  eputs ("s.stack_ptr -stack =");
  eputs (itoa (ps->stack_ptr - ps->stack));
  eputs ("\n");
  eputs ("4\n");

  for (file_struct ** p = ps->stack; p < ps->stack_ptr; p++)
    {
      eputs ((*p)->name);
      eputs ("\n");
    }

  eputs ("5\n");

  int i;
  i = ps->stack_ptr - ps->stack + STACK_SIZE;
  eputs ("i=");
  eputs (itoa (i));
  eputs ("\n");

  if (ps->stack_ptr >= ps->stack + STACK_SIZE)
    return 2;

  eputs ("6\n");
  struct file f2 = { "second.h" };
#if 0                           //__MESC__
  strcpy (f2.name, "second.h");
#endif

  *ps->stack_ptr = &f2;
  eputs ("7\n");
  ++ps->stack_ptr;
  eputs ("s.stack_ptr -stack =");
  eputs (itoa (ps->stack_ptr - ps->stack));
  eputs ("\n");

  for (file_struct ** p = ps->stack; p < ps->stack_ptr; p++)
    {
      eputs ((*p)->name);
      eputs ("\n");
    }

  if (ps->stack_ptr >= ps->stack + STACK_SIZE)
    return 0;
  struct file f3 = { "third.h" };
  *ps->stack_ptr = &f3;
  ++ps->stack_ptr;
  return 3;
}

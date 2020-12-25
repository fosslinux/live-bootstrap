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
#include <stdlib.h>
#include <string.h>

int
add (int a, int b)
{
  return a + b;
}

int
inc (int i)
{
  return i + 1;
}

struct scm
{
  int type;
  int car;
  int cdr;
};

int bla = 1234;
char g_arena[84];
#if __MESC__
struct scm *g_cells = g_arena;
#else
struct scm *g_cells = (struct scm *) g_arena;
#endif
char *g_chars = g_arena;

int
foo ()
{
  oputs ("t: foo\n");
  return 0;
};

int
bar (int i)
{
  oputs ("t: bar\n");
  return 0;
};

struct function
{
  int (*function) (void);
  int arity;
  //long arity;
  char *name;
};
struct function g_fun = { &exit, 1, "fun" };
struct function g_foo = { &foo, 0, "foo" };
struct function g_bar = { &bar, 1, "bar" };

void *functions[2];
//int functions[2];

struct function g_functions[2];
int g_function = 0;

enum type_t
{ TCHAR, TCLOSURE, TCONTINUATION, TFUNCTION, TKEYWORD, TMACRO, TNUMBER, TPAIR, TREF, TSPECIAL, TSTRING,
    TSYMBOL, TVALUES, TVECTOR, TBROKEN_HEART };

typedef int SCM;
int g_free = 3;
SCM tmp;
SCM tmp_num;

int ARENA_SIZE = 200;
#define TYPE(x) g_cells[x].type
#define CAR(x) g_cells[x].car
#define CDR(x) g_cells[x].cdr
#define VALUE(x) g_cells[x].cdr

#define CAAR(x) CAR (CAR (x))

struct scm scm_fun = { TFUNCTION, 0, 0 };

SCM cell_fun;


int
main ()
{
  oputs ("\n");
  oputs ("t: g_cells[0] = g_cells[1]\n");
  TYPE (1) = 1;
  CAR (1) = 2;
  CDR (1) = 3;
  g_cells[0] = g_cells[1];
  if (TYPE (0) != 1)
    return 1;
  if (CAR (0) != 2)
    return 2;
  if (CDR (0) != 3)
    return 3;

  oputs ("t: g_cells[i] = g_cells[j]\n");
  int i = 0;
  int j = 1;
  TYPE (1) = 4;
  CAR (1) = 5;
  CDR (1) = 6;
  g_cells[i] = g_cells[j];
  if (TYPE (0) != 4)
    return 4;
  if (CAR (0) != 5)
    return 5;
  if (CDR (0) != 6)
    return 6;

  oputs ("t: g_cells[0+add(0,0] = g_cells[0+inc(0)]\n");
  TYPE (1) = 1;
  CAR (1) = 2;
  CDR (1) = 3;
  g_cells[0 + add (0, 0)] = g_cells[0 + inc (0)];
  if (TYPE (0) != 1)
    return 7;
  if (CAR (0) != 2)
    return 9;
  if (CDR (0) != 3)
    return 9;

  g_cells[0].type = TNUMBER;
  g_cells[0].car = 0;
  g_cells[0].cdr = 0;
  g_cells[1].type = TNUMBER;
  g_cells[1].car = 0;
  g_cells[1].cdr = 0;

  oputs ("t: TYPE (0) == TYPE (1)\n");
  if (TYPE (0) == TYPE (1))
    goto ok;
  return 10;
ok:

  g_cells[0].car = 1;
  g_cells[1].car = 2;

  oputs ("t: int c = VALUE (0)\n");
  int c = CAR (0);
  if (c != 1)
    return 11;

  oputs ("t: CAAR (0) != 2\n");
  if (CAAR (0) != 2)
    return 12;

  oputs ("t: 2 != CAAR (0)\n");
  if (2 != CAAR (0))
    return 13;

  g_cells[3].type = 0x64;
  if (g_cells[3].type != 0x64)
    return g_cells[3].type;

  TYPE (4) = 4;
  if (TYPE (4) != 4)
    return 14;

  CDR (3) = 0x22;
  CDR (4) = 0x23;
  if (CDR (3) != 0x22)
    return 15;

  oputs ("t: g_fun.arity != 1;\n");
  if (g_fun.arity != 1)
    return 16;

  oputs ("t: g_fun.function != exit;\n");
  if (g_fun.function != &exit)
    return 17;

  oputs ("t: struct fun = {&exit,1,\"exit\"};\n");
  struct function fun = { &exit, 1, "exit" };

  oputs ("t: fun.arity != 1;\n");
  if (fun.arity != 1)
    return 18;

  oputs ("t: fun.function != exit;\n");
  if (fun.function != &exit)
    return 19;

  oputs ("t: oputs (fun.name)\n");
  if (strcmp (fun.name, "exit"))
    return 20;

  oputs ("t: oputs (g_fun.name)\n");
  if (strcmp (g_fun.name, "fun"))
    return 21;

  oputs ("t: g_functions[g_function++] = g_foo;\n");
  g_functions[g_function++] = g_foo;

  oputs ("t: pbar->arity == 1\n");
  struct function *barp = &g_bar;
  if (barp->arity != 1)
    return 22;

  int fn = 0;
  oputs ("t: g_functions[g_cells[fn].cdr].arity\n");
  if (g_functions[g_cells[fn].cdr].arity)
    return 23;
  if (g_functions[g_cells[fn].cdr].arity != 0)
    return 24;











  int (*functionx) (void) = 0;
  functionx = g_functions[0].function;
  oputs ("t: functionx == foo\n");
  if (functionx != foo)
    return 25;

  oputs ("t: g_functions[0].name\n");
  if (strcmp (g_functions[0].name, "foo"))
    return 26;

  oputs ("t: (functionx) () == foo\n");
  if ((functionx) () != 0)
    return 27;

  oputs ("t: g_functions[<foo>].arity\n");
  if (g_functions[0].arity != 0)
    return 28;

  fn++;
  g_functions[fn] = g_bar;
  g_cells[fn].cdr = fn;
  if (g_cells[fn].cdr != fn)
    return 29;

  oputs ("t: g_functions[g_cells[fn].cdr].function\n");
  functionx = g_functions[g_cells[fn].cdr].function;

  oputs ("t: g_functions[1].name\n");
  if (strcmp (g_functions[1].name, "bar"))
    return 30;

  oputs ("t: functionx == bar\n");
  if (functionx != bar)
    return 31;

  oputs ("t: (functiony) (1) == bar\n");
  int (*functiony) (int) = 0;
  functiony = g_functions[g_cells[fn].cdr].function;
  if ((functiony) (1) != 0)
    return 32;

  oputs ("t: g_functions[<bar>].arity\n");
  if (g_functions[fn].arity != 1)
    return 33;

  // fake name
  scm_fun.car = 33;
  scm_fun.cdr = g_function;
  g_function++;
  oputs ("fun");
  g_functions[g_function] = g_fun;

  cell_fun = g_free++;
  g_cells[cell_fun] = scm_fun;

  oputs ("t: TYPE (cell_fun)\n");
  if (TYPE (cell_fun) != TFUNCTION)
    return 34;

  oputs ("t: CAR (cell_fun)\n");
  if (CAR (cell_fun) != 33)
    return 35;

  // FIXME!
  // oputs ("t: CDR (cell_fun)\n");
  // if (CDR (cell_fun) != g_function)
  //   return 36;

  return 0;
}

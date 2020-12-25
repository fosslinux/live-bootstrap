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

struct scm
{
  int type;
  int car;
  int cdr;
};

int bla = 1234;
char g_arena[84];
struct scm *g_cells = (struct scm *) g_arena;
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
  char *name;
};
struct function g_fun = { &exit, 1, "fun" };
struct function g_foo = { &foo, 0, "foo" };
struct function g_bar = { &bar, 1, "bar" };

//void *functions[2];
int functions[2];

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

SCM
alloc (int n)
{
  oputs ("040\n");
  SCM x = g_free;
  g_free += n;
  return x;
}

SCM
make_cell (SCM type, SCM car, SCM cdr)
{
  oputs ("030\n");
  SCM x = alloc (1);
  TYPE (x) = VALUE (type);
  if (VALUE (type) == TCHAR || VALUE (type) == TNUMBER)
    {
      if (car)
        CAR (x) = CAR (car);
      if (cdr)
        CDR (x) = CDR (cdr);
    }
  else if (VALUE (type) == TFUNCTION)
    {
      if (car)
        CAR (x) = car;
      if (cdr)
        CDR (x) = CDR (cdr);
    }
  else
    {
      CAR (x) = car;
      CDR (x) = cdr;
    }
  return x;
}

SCM
make_cell_test ()
{
  oputs ("010\n");
  VALUE (tmp_num) = TPAIR;
  oputs ("011\n");
  make_cell (tmp_num, 0, 1);
  oputs ("012\n");
  return 0;
}

SCM
make_tmps_test (struct scm * cells)
{
  oputs ("t: tmp = g_free++\n");
  tmp = g_free++;
  oputs ("t: cells[tmp].type = CHAR\n");
  cells[tmp].type = TCHAR;
  oputs ("000\n");
  tmp_num = g_free++;
  oputs ("001\n");
  cells[tmp_num].type = TNUMBER;
  oputs ("002\n");

  return 0;
}

int
main ()
{
  oputs ("\n");
  make_tmps_test (g_cells);
  make_cell_test ();
  oputs ("020\n");
  return 0;
}

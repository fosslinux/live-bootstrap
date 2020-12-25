/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#if SYSTEM_LIBC
#error "SYSTEM_LIBC not supported"
#endif

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <mes/lib.h>

char arena[2000];

typedef int SCM;

int g_debug = 0;
int g_free = 0;

SCM g_continuations = 0;
SCM g_symbols = 0;
SCM g_stack = 0;
SCM r0 = 0;                     // a/env
SCM r1 = 0;                     // param 1
SCM r2 = 0;                     // save 2+load/dump
SCM r3 = 0;                     // continuation

enum type_t
{ TCHAR, TCLOSURE, TCONTINUATION, TFUNCTION, TKEYWORD, TMACRO, TNUMBER, TPAIR, TREF, TSPECIAL, TSTRING,
    TSYMBOL, TVALUES, TVECTOR, TBROKEN_HEART };

struct scm
{
  enum type_t type;
  SCM car;
  SCM cdr;
};

struct function
{
  int (*function) (void);
  int arity;
  char *name;
};

#if __MESC__
struct scm *g_cells = arena;
#else
struct scm *g_cells = (struct scm *) arena;
#endif

#define cell_nil 1
#define cell_f 2
#define cell_t 3
#define cell_dot 4
// #define cell_arrow 5
#define cell_undefined 6
#define cell_unspecified 7
#define cell_closure 8
#define cell_circular 9
#define cell_begin 10
#define cell_symbol_dot 11
#define cell_symbol_lambda 12
#define cell_symbol_begin 13
#define cell_symbol_if 14
#define cell_symbol_quote 15
#define cell_symbol_set_x 16

#define cell_vm_apply 45
#define cell_vm_apply2 46

#define cell_vm_eval 47

#define cell_vm_begin 56
//#define cell_vm_begin_read_input_file 57
#define cell_vm_begin2 58

#define cell_vm_return 63

SCM tmp;
SCM tmp_num;
SCM tmp_num2;

int ARENA_SIZE = 200;
struct function g_functions[5];
int g_function = 0;


SCM make_cell_ (SCM type, SCM car, SCM cdr);
struct function fun_make_cell_ = { &make_cell_, 3, "core:make-cell" };
struct scm scm_make_cell_ = { TFUNCTION, 0, 0 };

   //, "core:make-cell", 0};
SCM cell_make_cell_;

SCM cons (SCM x, SCM y);
struct function fun_cons = { &cons, 2, "cons" };
struct scm scm_cons = { TFUNCTION, 0, 0 };

  // "cons", 0};
SCM cell_cons;

SCM car (SCM x);
struct function fun_car = { &car, 1, "car" };
struct scm scm_car = { TFUNCTION, 0, 0 };

  // "car", 0};
SCM cell_car;

SCM cdr (SCM x);
struct function fun_cdr = { &cdr, 1, "cdr" };
struct scm scm_cdr = { TFUNCTION, 0, 0 };

// "cdr", 0};
SCM cell_cdr;

// SCM eq_p (SCM x, SCM y);
// struct function fun_eq_p = {&eq_p,2,"eq?"};
// scm scm_eq_p = {TFUNCTION,0,0};
// SCM cell_eq_p;

#define TYPE(x) (g_cells[x].type)

#define CAR(x) g_cells[x].car
#define LENGTH(x) g_cells[x].car
#define STRING(x) g_cells[x].car

#define CDR(x) g_cells[x].cdr
#define CONTINUATION(x) g_cells[x].cdr

#define FUNCTION(x) g_functions[g_cells[x].cdr]
#define VALUE(x) g_cells[x].cdr
#define VECTOR(x) g_cells[x].cdr

#define MAKE_CHAR(n) make_cell_ (tmp_num_ (TCHAR), 0, tmp_num2_ (n))
#define MAKE_NUMBER(n) make_cell_ (tmp_num_ (TNUMBER), 0, tmp_num2_ (n))

#define CAAR(x) CAR (CAR (x))
#define CADAR(x) CAR (CDR (CAR (x)))
#define CDADAR(x) CAR (CDR (CAR (CDR (x))))
#define CADR(x) CAR (CDR (x))

#define MAKE_STRING(x) make_cell_ (tmp_num_ (TSTRING), x, 0)

SCM
alloc (int n)
{
  assert (g_free + n < ARENA_SIZE);
  SCM x = g_free;
  g_free += n;
  return x;
}

SCM
make_cell_ (SCM type, SCM car, SCM cdr)
{
  SCM x = alloc (1);
  assert (TYPE (type) == TNUMBER);
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
tmp_num_ (int x)
{
  VALUE (tmp_num) = x;
  return tmp_num;
}

SCM
tmp_num2_ (int x)
{
  VALUE (tmp_num2) = x;
  return tmp_num2;
}

SCM
cons (SCM x, SCM y)
{
  VALUE (tmp_num) = TPAIR;
  return make_cell_ (tmp_num, x, y);
}

SCM
car (SCM x)
{
  return CAR (x);
}

SCM
cdr (SCM x)
{
  return CDR (x);
}

SCM
gc_push_frame ()
{
  SCM frame = cons (r1, cons (r2, cons (r3, cons (r0, cell_nil))));
  g_stack = cons (frame, g_stack);
  return g_stack;
}

SCM
append2 (SCM x, SCM y)
{
  if (x == cell_nil)
    return y;
  assert (TYPE (x) == TPAIR);
  return cons (car (x), append2 (cdr (x), y));
}

SCM
pairlis (SCM x, SCM y, SCM a)
{
  if (x == cell_nil)
    return a;
  if (TYPE (x) != TPAIR)
    return cons (cons (x, y), a);
  return cons (cons (car (x), car (y)), pairlis (cdr (x), cdr (y), a));
}

SCM
assq (SCM x, SCM a)
{
  while (a != cell_nil && x == CAAR (a))
    a = CDR (a);
  return a != cell_nil ? car (a) : cell_f;
}

SCM
push_cc (SCM p1, SCM p2, SCM a, SCM c)  ///((internal))
{
  puts ("push cc\n");
  SCM x = r3;
  r3 = c;
  r2 = p2;
  gc_push_frame ();
  r1 = p1;
  r0 = a;
  r3 = x;
  return cell_unspecified;
}

SCM
caar (SCM x)
{
  return car (car (x));
}

SCM
cadr (SCM x)
{
  return car (cdr (x));
}

SCM
cdar (SCM x)
{
  return cdr (car (x));
}

SCM
cddr (SCM x)
{
  return cdr (cdr (x));
}

#if __GNUC__
//FIXME
SCM call (SCM, SCM);
SCM gc_pop_frame ();
#endif

SCM
eval_apply ()
{
eval_apply:
  switch (r3)
    {
    case cell_vm_apply:
      {
        goto apply;
      }
    case cell_unspecified:
      {
        return r1;
      }
    }

  SCM x = cell_nil;
  SCM y = cell_nil;

apply:
  switch (TYPE (car (r1)))
    {
    case TFUNCTION:
      {
        puts ("apply.function\n");
        r1 = call (car (r1), cdr (r1));
        goto vm_return;
      }
    }
vm_return:
  x = r1;
  gc_pop_frame ();
  r1 = x;
  goto eval_apply;
}

SCM
call (SCM fn, SCM x)
{
  puts ("call\n");
  if ((FUNCTION (fn).arity > 0 || FUNCTION (fn).arity == -1) && x != cell_nil && TYPE (CAR (x)) == TVALUES)
    x = cons (CADAR (x), CDR (x));
  if ((FUNCTION (fn).arity > 1 || FUNCTION (fn).arity == -1)
      && x != cell_nil && TYPE (CDR (x)) == TPAIR && TYPE (CADR (x)) == TVALUES)
    x = cons (CAR (x), cons (CDADAR (x), CDR (x)));
  switch (FUNCTION (fn).arity)
    {
    case 0:
      {
        return (FUNCTION (fn).function) ();
      }
    case 1:
      {
        return ((SCM (*)(SCM)) (FUNCTION (fn).function)) (car (x));
      }
    case 2:
      {
        return ((SCM (*)(SCM, SCM)) (FUNCTION (fn).function)) (car (x), cadr (x));
      }
    case 3:
      {
        return ((SCM (*)(SCM, SCM, SCM)) (FUNCTION (fn).function)) (car (x), cadr (x), car (cddr (x)));
      }
    case -1:
      {
        return ((SCM (*)(SCM)) (FUNCTION (fn).function)) (x);
      }
    }
  return cell_unspecified;
}

SCM
gc_peek_frame ()
{
  SCM frame = car (g_stack);
  r1 = car (frame);
  r2 = cadr (frame);
  r3 = car (cddr (frame));
  r0 = cadr (cddr (frame));
  return frame;
}

SCM
gc_pop_frame ()
{
  SCM frame = gc_peek_frame (g_stack);
  g_stack = cdr (g_stack);
  return frame;
}

SCM
mes_g_stack (SCM a)             ///((internal))
{
  r0 = a;
  r1 = MAKE_CHAR (0);
  r2 = MAKE_CHAR (0);
  r3 = MAKE_CHAR (0);
  g_stack = cons (cell_nil, cell_nil);
  return r0;
}

// Environment setup
SCM
make_tmps (struct scm * cells)
{
  tmp = g_free++;
  cells[tmp].type = TCHAR;
  tmp_num = g_free++;
  cells[tmp_num].type = TNUMBER;
  tmp_num2 = g_free++;
  cells[tmp_num2].type = TNUMBER;
  return 0;
}

SCM
make_symbol_ (SCM s)
{
  VALUE (tmp_num) = TSYMBOL;
  SCM x = make_cell_ (tmp_num, s, 0);
  g_symbols = cons (x, g_symbols);
  return x;
}

SCM
make_symbol (SCM s)
{
  SCM x = 0;
  return x ? x : make_symbol_ (s);
}

SCM
acons (SCM key, SCM value, SCM alist)
{
  return cons (cons (key, value), alist);
}

// Jam Collector
SCM g_symbol_max;

SCM
gc_init_cells ()
{
  return 0;
}

// INIT NEWS

SCM
mes_symbols ()                  ///((internal))
{
  gc_init_cells ();
  //  gc_init_news ();

#if __GNUC__ && 0
  //#include "mes.symbols.i"
#else
  g_free++;
// g_cells[cell_nil] = scm_nil;

  g_free++;
// g_cells[cell_f] = scm_f;

  g_free++;
// g_cells[cell_t] = scm_t;

  g_free++;
// g_cells[cell_dot] = scm_dot;

  g_free++;
// g_cells[cell_arrow] = scm_arrow;

  g_free++;
// g_cells[cell_undefined] = scm_undefined;

  g_free++;
// g_cells[cell_unspecified] = scm_unspecified;

  g_free++;
// g_cells[cell_closure] = scm_closure;

  g_free++;
// g_cells[cell_circular] = scm_circular;

  g_free++;
// g_cells[cell_begin] = scm_begin;

///
  g_free = 44;
  g_free++;
// g_cells[cell_vm_apply] = scm_vm_apply;

  g_free++;
// g_cells[cell_vm_apply2] = scm_vm_apply2;

  g_free++;
// g_cells[cell_vm_eval] = scm_vm_eval;

///
  g_free = 55;
  g_free++;
// g_cells[cell_vm_begin] = scm_vm_begin;

  g_free++;
// g_cells[cell_vm_begin_read_input_file] = scm_vm_begin_read_input_file;

  g_free++;
// g_cells[cell_vm_begin2] = scm_vm_begin2;

///
  g_free = 62;
  g_free++;
// g_cells[cell_vm_return] = scm_vm_return;

#endif

  g_symbol_max = g_free;
  make_tmps (g_cells);

  g_symbols = 0;
  for (int i = 1; i < g_symbol_max; i++)
    g_symbols = cons (i, g_symbols);

  SCM a = cell_nil;

  a = acons (cell_symbol_dot, cell_dot, a);
  a = acons (cell_symbol_begin, cell_begin, a);
  a = acons (cell_closure, a, a);

  return a;
}

SCM
make_closure (SCM args, SCM body, SCM a)
{
  return make_cell_ (tmp_num_ (TCLOSURE), cell_f, cons (cons (cell_circular, a), cons (args, body)));
}

SCM
mes_environment ()              ///((internal))
{
  SCM a = 0;
  a = mes_symbols ();
  a = mes_g_stack (a);
  return a;
}

SCM
mes_builtins (SCM a)
{
#if 0
  //__GNUC__
//#include "mes.i"

// #include "lib.i"
// #include "math.i"
// #include "posix.i"
// #include "reader.i"

// #include "lib.environment.i"
// #include "math.environment.i"
// #include "mes.environment.i"
// #include "posix.environment.i"
// #include "reader.environment.i"
#else
  scm_make_cell_.cdr = g_function;
  g_functions[g_function++] = fun_make_cell_;
  cell_make_cell_ = g_free++;
  g_cells[cell_make_cell_] = scm_make_cell_;

  scm_cons.cdr = g_function;
  g_functions[g_function++] = fun_cons;
  cell_cons = g_free++;
  g_cells[cell_cons] = scm_cons;

  scm_car.cdr = g_function;
  g_functions[g_function++] = fun_car;
  cell_car = g_free++;
  g_cells[cell_car] = scm_car;

  scm_cdr.cdr = g_function;
  g_functions[g_function++] = fun_cdr;
  cell_cdr = g_free++;
  g_cells[cell_cdr] = scm_cdr;
#endif
  return a;
}

SCM
bload_env (SCM a)               ///((internal))
{
  __stdin = open ("module/mes/read-0.mo", 0);
  char *p = (char *) g_cells;
  assert (getchar () == 'M');
  assert (getchar () == 'E');
  assert (getchar () == 'S');
  g_stack = getchar () << 8;
  g_stack += getchar ();
  int c = getchar ();
  while (c != EOF)
    {
      *p++ = c;
      c = getchar ();
    }
  g_free = (p - (char *) g_cells) / sizeof (struct scm);
  gc_peek_frame ();
  g_symbols = r1;
  __stdin = STDIN;
  r0 = mes_builtins (r0);
  return r2;
}

SCM
fill ()
{
  TYPE (0) = 0x6c6c6168;
  CAR (0) = 0x6a746f6f;
  CDR (0) = 0x00002165;

  TYPE (1) = TSYMBOL;
  CAR (1) = 0x2d2d2d2d;
  CDR (1) = 0x3e3e3e3e;

  TYPE (9) = 0x2d2d2d2d;
  CAR (9) = 0x2d2d2d2d;
  CDR (9) = 0x3e3e3e3e;

  // (cons 0 1)
  TYPE (10) = TPAIR;
  CAR (10) = 11;
  CDR (10) = 12;

  TYPE (11) = TFUNCTION;
  CAR (11) = 0x58585858;
  // 0 = make_cell_
  // 1 = cons
  // 2 = car
  CDR (11) = 1;

  TYPE (12) = TPAIR;
  CAR (12) = 13;
  //CDR (12) = 1;
  CDR (12) = 14;

  TYPE (13) = TNUMBER;
  CAR (13) = 0x58585858;
  CDR (13) = 0;

  TYPE (14) = TPAIR;
  CAR (14) = 15;
  CDR (14) = 1;

  TYPE (15) = TNUMBER;
  CAR (15) = 0x58585858;
  CDR (15) = 1;

  return 0;
}

SCM
display_ (SCM x)
{
  //puts ("<display>\n");
  switch (TYPE (x))
    {
    case TCHAR:
      {
        //puts ("<char>\n");
        puts ("#\\");
        putchar (VALUE (x));
        break;
      }
    case TFUNCTION:
      {
        //puts ("<function>\n");
        if (VALUE (x) == 0)
          puts ("core:make-cell");
        if (VALUE (x) == 1)
          puts ("cons");
        if (VALUE (x) == 2)
          puts ("car");
        if (VALUE (x) == 3)
          puts ("cdr");
        break;
      }
    case TNUMBER:
      {
        //puts ("<number>\n");
#if __GNUC__
        puts (itoa (VALUE (x)));
#else
        int i;
        i = VALUE (x);
        i = i + 48;
        putchar (i);
#endif
        break;
      }
    case TPAIR:
      {
        //puts ("<pair>\n");
        //if (cont != cell_f) puts "(");
        puts ("(");
        if (x && x != cell_nil)
          display_ (CAR (x));
        if (CDR (x) && CDR (x) != cell_nil)
          {
#if __GNUC__
            if (TYPE (CDR (x)) != TPAIR)
              puts (" . ");
#else
            int c;
            c = CDR (x);
            c = TYPE (c);
            if (c != TPAIR)
              puts (" . ");
#endif
            display_ (CDR (x));
          }
        //if (cont != cell_f) puts (")");
        puts (")");
        break;
      }
    case TSPECIAL:
      {
        switch (x)
          {
          case 1:
            {
              puts ("()");
              break;
            }
          case 2:
            {
              puts ("#f");
              break;
            }
          case 3:
            {
              puts ("#t");
              break;
            }
          default:
            {
#if __GNUC__
              puts ("<x:");
              puts (itoa (x));
              puts (">");
#else
              puts ("<x>");
#endif
            }
          }
        break;
      }
    case TSYMBOL:
      {
        switch (x)
          {
          case 11:
            {
              puts (" . ");
              break;
            }
          case 12:
            {
              puts ("lambda");
              break;
            }
          case 13:
            {
              puts ("begin");
              break;
            }
          case 14:
            {
              puts ("if");
              break;
            }
          case 15:
            {
              puts ("quote");
              break;
            }
          case 37:
            {
              puts ("car");
              break;
            }
          case 38:
            {
              puts ("cdr");
              break;
            }
          case 39:
            {
              puts ("null?");
              break;
            }
          case 40:
            {
              puts ("eq?");
              break;
            }
          case 41:
            {
              puts ("cons");
              break;
            }
          default:
            {
#if __GNUC__
              puts ("<s:");
              puts (itoa (x));
              puts (">");
#else
              puts ("<s>");
#endif
            }
          }
        break;
      }
    default:
      {
        //puts ("<default>\n");
#if __GNUC__
        puts ("<");
        puts (itoa (TYPE (x)));
        puts (":");
        puts (itoa (x));
        puts (">");
#else
        puts ("_");
#endif
        break;
      }
    }
  return 0;
}

SCM
simple_bload_env (SCM a)        ///((internal))
{
  puts ("reading: ");
  char *mo = "module/mes/tiny-0-32.mo";
  puts (mo);
  puts ("\n");
  __stdin = open (mo, 0);
  if (__stdin < 0)
    {
      eputs ("no such file: module/mes/tiny-0-32.mo\n");
      return 1;
    }

  char *p = (char *) g_cells;
  int c;

  assert (getchar () == 'M');
  assert (getchar () == 'E');
  assert (getchar () == 'S');
  puts (" *GOT MES*\n");

  g_stack = getchar () << 8;
  g_stack += getchar ();

  puts ("stack: ");
  puts (itoa (g_stack));
  puts ("\n");

  c = getchar ();
  while (c != -1)
    {
      *p++ = c;
      c = getchar ();
    }

  puts ("read done\n");

  g_free = (p - (char *) g_cells) / sizeof (struct scm);

  if (g_free != 15)
    exit (33);

  g_symbols = 1;

  __stdin = STDIN;
  r0 = mes_builtins (r0);

  if (g_free != 19)
    exit (34);

  puts ("cells read: ");
  puts (itoa (g_free));
  puts ("\n");

  puts ("symbols: ");
  puts (itoa (g_symbols));
  puts ("\n");
  // display_ (g_symbols);
  // puts ("\n");

  display_ (10);
  puts ("\n");

  fill ();
  r2 = 10;

  if (TYPE (12) != TPAIR)
    exit (33);

  puts ("program[");
  puts (itoa (r2));
  puts ("]: ");

  display_ (r2);
  //display_ (14);
  puts ("\n");

  r0 = 1;
  //r2 = 10;
  return r2;
}

int
main (int argc, char *argv[])
{
  puts ("Hello cons-mes!\n");
  if (argc > 1 && !strcmp (argv[1], "--help"))
    return eputs ("Usage: mes [--dump|--load] < FILE");
#if __GNUC__
  if (argc > 1 && !strcmp (argv[1], "--version"))
    {
      eputs ("Mes ");
      return eputs (MES_VERSION);
    };
#else
  if (argc > 1 && !strcmp (argv[1], "--version"))
    {
      eputs ("Mes ");
      return eputs ("0.4");
    };
#endif
  __stdin = STDIN;

  r0 = mes_environment ();

  SCM program = simple_bload_env (r0);

  puts ("g_free=");
  puts (itoa (g_free));
  puts ("\n");

  push_cc (r2, cell_unspecified, r0, cell_unspecified);

  puts ("g_free=");
  puts (itoa (g_free));
  puts ("\n");

  puts ("g_stack=");
  puts (itoa (g_stack));
  puts ("\n");

  puts ("r0=");
  puts (itoa (r0));
  puts ("\n");

  puts ("r1=");
  puts (itoa (r1));
  puts ("\n");

  puts ("r2=");
  puts (itoa (r2));
  puts ("\n");

  puts ("r3=");
  puts (itoa (r3));
  puts ("\n");

  r3 = cell_vm_apply;
  r1 = eval_apply ();
  display_ (r1);

  eputs ("\n");
  return 0;
}

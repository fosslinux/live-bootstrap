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

#include <mes/lib.h>

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>

char arena[300];

typedef int SCM;

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

#if __MESC__
struct scm *g_cells = arena;
#else
struct scm *g_cells = (struct scm *) arena;
#endif

#define cell_nil 1
#define cell_f 2
#define cell_t 3

#define TYPE(x) (g_cells[x].type)

#define CAR(x) g_cells[x].car

#define CDR(x) g_cells[x].cdr
#define VALUE(x) g_cells[x].cdr

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

// Environment setup

SCM
mes_environment ()
{
  return 0;
}

SCM
mes_builtins (SCM a)
{
  return a;
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

  // (A(B))
  TYPE (10) = TPAIR;
  CAR (10) = 11;
  CDR (10) = 12;

  TYPE (11) = TCHAR;
  CAR (11) = 0x58585858;
  CDR (11) = 89;

  TYPE (12) = TPAIR;
  CAR (12) = 13;
  CDR (12) = 1;

  TYPE (13) = TCHAR;
  CAR (11) = 0x58585858;
  CDR (13) = 90;

  TYPE (14) = 0x58585858;
  CAR (14) = 0x58585858;
  CDR (14) = 0x58585858;

  TYPE (14) = 0x58585858;
  CAR (14) = 0x58585858;
  CDR (14) = 0x58585858;

  TYPE (16) = 0x3c3c3c3c;
  CAR (16) = 0x2d2d2d2d;
  CDR (16) = 0x2d2d2d2d;
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
bload_env (SCM a)               ///((internal))
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

  // BOOM
  //char *p = arena;
  char *p = (char *) g_cells;
  int c;

  c = getchar ();
  putchar (c);
  if (c != 'M')
    exit (10);
  c = getchar ();
  putchar (c);
  if (c != 'E')
    exit (11);
  c = getchar ();
  putchar (c);
  if (c != 'S')
    exit (12);
  puts (" *GOT MES*\n");

  // skip stack
  getchar ();
  getchar ();

  int i = 0;
  c = getchar ();
  while (c != -1)
    {
      i++;
      eputs (itoa (i));
      eputs (": ");
      eputs (itoa (c));
      eputs ("\n");
      *p++ = c;
      c = getchar ();
    }

  puts ("read done\n");
  display_ (10);

  puts ("\n");
  return r2;
}

int
main (int argc, char *argv[])
{
  fill ();
  char *p = arena;
  puts (p);
  puts ("\n");
  display_ (10);
  puts ("\n");
  SCM program = bload_env (r0);

  return 0;
}

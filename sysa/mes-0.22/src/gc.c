/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include "mes/lib.h"
#include "mes/mes.h"

#include <assert.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

long ARENA_SIZE;
long MAX_ARENA_SIZE;
long STACK_SIZE;
long JAM_SIZE;
long GC_SAFETY;
long MAX_STRING;
char *g_arena;
long g_free;
SCM g_stack;
SCM *g_stack_array;
struct scm *g_cells;
struct scm *g_news;

SCM
gc_init ()                ///((internal))
{
#if SYSTEM_LIBC
  ARENA_SIZE = 100000000;    // 2.3GiB
#else
  ARENA_SIZE = 300000;       // 32b: 3MiB, 64b: 6 MiB
#endif
  MAX_ARENA_SIZE = 100000000;
  STACK_SIZE = 20000;

  JAM_SIZE = 20000;
  GC_SAFETY = 2000;
  MAX_STRING = 524288;

  char *p;
  if (p = getenv ("MES_MAX_ARENA"))
    MAX_ARENA_SIZE = atoi (p);
  if (p = getenv ("MES_ARENA"))
    ARENA_SIZE = atoi (p);
  JAM_SIZE = ARENA_SIZE / 10;
  if (p = getenv ("MES_JAM"))
    JAM_SIZE = atoi (p);
  GC_SAFETY = ARENA_SIZE / 100;
  if (p = getenv ("MES_SAFETY"))
    GC_SAFETY = atoi (p);
  if (p = getenv ("MES_STACK"))
    STACK_SIZE = atoi (p);
  if (p = getenv ("MES_MAX_STRING"))
    MAX_STRING = atoi (p);

  long arena_bytes = (ARENA_SIZE + JAM_SIZE) * sizeof (struct scm);
  void *a = malloc (arena_bytes + STACK_SIZE * sizeof (SCM));
  g_cells = (struct scm *) a;
  g_stack_array = (SCM *) (a + arena_bytes);

  TYPE (0) = TVECTOR;
  LENGTH (0) = 1000;
  VECTOR (0) = 0;
  g_cells++;
  TYPE (0) = TCHAR;
  VALUE (0) = 'c';

  // FIXME: remove MES_MAX_STRING, grow dynamically
  g_buf = (char *) malloc (MAX_STRING);

  return 0;
}

SCM
gc_init_news ()                 ///((internal))
{
  g_news = g_cells + g_free;
  NTYPE (0) = TVECTOR;
  NLENGTH (0) = 1000;
  NVECTOR (0) = 0;
  g_news++;
  NTYPE (0) = TCHAR;
  NVALUE (0) = 'n';
  return 0;
}

SCM
gc_up_arena ()                  ///((internal))
{
  long old_arena_bytes = (ARENA_SIZE + JAM_SIZE) * sizeof (struct scm);
  if (ARENA_SIZE >> 1 < MAX_ARENA_SIZE >> 2)
    {
      ARENA_SIZE <<= 1;
      JAM_SIZE <<= 1;
      GC_SAFETY <<= 1;
    }
  else
    ARENA_SIZE = MAX_ARENA_SIZE - JAM_SIZE;
  long arena_bytes = (ARENA_SIZE + JAM_SIZE) * sizeof (struct scm);
  void *p = realloc (g_cells - 1, arena_bytes + STACK_SIZE * sizeof (SCM));
  if (!p)
    {
      eputs ("realloc failed, g_free=");
      eputs (itoa (g_free));
      eputs (":");
      eputs (itoa (ARENA_SIZE - g_free));
      eputs ("\n");
      assert (0);
      exit (1);
    }
  g_cells = (struct scm *) p;
  memcpy (p + arena_bytes, p + old_arena_bytes, STACK_SIZE * sizeof (SCM));
  g_cells++;

  return 0;
}

void
gc_flip ()                      ///((internal))
{
  if (g_debug > 2)
    {
      eputs (";;;   => jam[");
      eputs (itoa (g_free));
      eputs ("]\n");
    }
  if (g_free > JAM_SIZE)
    JAM_SIZE = g_free + g_free / 2;
  memcpy (g_cells - 1, g_news - 1, (g_free + 2) * sizeof (struct scm));
}

SCM
gc_copy (SCM old)               ///((internal))
{
  if (TYPE (old) == TBROKEN_HEART)
    return g_cells[old].car;
  SCM new = g_free++;
  g_news[new] = g_cells[old];
  if (NTYPE (new) == TSTRUCT || NTYPE (new) == TVECTOR)
    {
      NVECTOR (new) = g_free;
      for (long i = 0; i < LENGTH (old); i++)
        g_news[g_free++] = g_cells[VECTOR (old) + i];
    }
  else if (NTYPE (new) == TBYTES)
    {
      char const *src = CBYTES (old);
      char *dest = NCBYTES (new);
      size_t length = NLENGTH (new);
      memcpy (dest, src, length + 1);
      g_free += bytes_cells (length) - 1;

      if (g_debug > 4)
        {
          eputs ("gc copy bytes: ");
          eputs (src);
          eputs ("\n");
          eputs ("    length: ");
          eputs (itoa (LENGTH (old)));
          eputs ("\n");
          eputs ("    nlength: ");
          eputs (itoa (NLENGTH (new)));
          eputs ("\n");
          eputs ("        ==> ");
          eputs (dest);
          eputs ("\n");
        }
    }
  TYPE (old) = TBROKEN_HEART;
  CAR (old) = new;
  return new;
}

SCM
gc_relocate_car (SCM new, SCM car)      ///((internal))
{
  g_news[new].car = car;
  return cell_unspecified;
}

SCM
gc_relocate_cdr (SCM new, SCM cdr)      ///((internal))
{
  g_news[new].cdr = cdr;
  return cell_unspecified;
}

void
gc_loop (SCM scan)              ///((internal))
{
  SCM car;
  SCM cdr;
  while (scan < g_free)
    {
      if (NTYPE (scan) == TBROKEN_HEART)
        error (cell_symbol_system_error, cstring_to_symbol ("gc"));
      if (NTYPE (scan) == TMACRO || NTYPE (scan) == TPAIR || NTYPE (scan) == TREF || scan == 1  // null
          || NTYPE (scan) == TVARIABLE)
        {
          car = gc_copy (g_news[scan].car);
          gc_relocate_car (scan, car);
        }
      if ((NTYPE (scan) == TCLOSURE || NTYPE (scan) == TCONTINUATION || NTYPE (scan) == TKEYWORD || NTYPE (scan) == TMACRO || NTYPE (scan) == TPAIR || NTYPE (scan) == TPORT || NTYPE (scan) == TSPECIAL || NTYPE (scan) == TSTRING || NTYPE (scan) == TSYMBOL || scan == 1     // null
           || NTYPE (scan) == TVALUES) && g_news[scan].cdr)     // allow for 0 terminated list of symbols
        {
          cdr = gc_copy (g_news[scan].cdr);
          gc_relocate_cdr (scan, cdr);
        }
      if (NTYPE (scan) == TBYTES)
        scan += bytes_cells (NLENGTH (scan)) - 1;
      scan++;
    }
  gc_flip ();
}

SCM
gc_check ()
{
  if (g_free + GC_SAFETY > ARENA_SIZE)
    gc ();
  return cell_unspecified;
}

SCM
gc_ ()                          ///((internal))
{
  gc_init_news ();
  if (g_debug == 2)
    eputs (".");
  if (g_debug > 2)
    {
      eputs (";;; gc[");
      eputs (itoa (g_free));
      eputs (":");
      eputs (itoa (ARENA_SIZE - g_free));
      eputs ("]...");
    }
  g_free = 1;

#if __MESC__
  if (ARENA_SIZE < MAX_ARENA_SIZE && (long) g_news > 0)
#else
  if (ARENA_SIZE < MAX_ARENA_SIZE && g_news > 0)
#endif
    {
      if (g_debug == 2)
        eputs ("+");
      if (g_debug > 2)
        {
          eputs (" up[");
          eputs (itoa ((unsigned long) g_cells));
          eputs (",");
          eputs (itoa ((unsigned long) g_news));
          eputs (":");
          eputs (itoa (ARENA_SIZE));
          eputs (",");
          eputs (itoa (MAX_ARENA_SIZE));
          eputs ("]...");
        }
      gc_up_arena ();
    }

  for (long i = g_free; i < g_symbol_max; i++)
    gc_copy (i);
  g_symbols = gc_copy (g_symbols);
  g_macros = gc_copy (g_macros);
  g_ports = gc_copy (g_ports);
  m0 = gc_copy (m0);
  for (long i = g_stack; i < STACK_SIZE; i++)
    g_stack_array[i] = gc_copy (g_stack_array[i]);
  gc_loop (1);
}

SCM
gc ()
{
  if (g_debug > 5)
    {
      eputs ("symbols: ");
      write_error_ (g_symbols);
      eputs ("\n");
      eputs ("R0: ");
      write_error_ (r0);
      eputs ("\n");
    }
  gc_push_frame ();
  gc_ ();
  gc_pop_frame ();
  if (g_debug > 5)
    {
      eputs ("symbols: ");
      write_error_ (g_symbols);
      eputs ("\n");
      eputs ("R0: ");
      write_error_ (r0);
      eputs ("\n");
    }
}

SCM
gc_push_frame ()                ///((internal))
{
  if (g_stack < 5)
    assert (!"STACK FULL");
  g_stack_array[--g_stack] = cell_f;
  g_stack_array[--g_stack] = r0;
  g_stack_array[--g_stack] = r1;
  g_stack_array[--g_stack] = r2;
  g_stack_array[--g_stack] = r3;
  return g_stack;
}

SCM
gc_peek_frame ()                ///((internal))
{
  r3 = g_stack_array[g_stack];
  r2 = g_stack_array[g_stack + 1];
  r1 = g_stack_array[g_stack + 2];
  r0 = g_stack_array[g_stack + 3];
  return g_stack_array[g_stack + FRAME_PROCEDURE];
}

SCM
gc_pop_frame ()                 ///((internal))
{
  SCM x = gc_peek_frame ();
  g_stack += 5;
  return x;
}

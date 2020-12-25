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
#include <stdlib.h>
#include <string.h>

struct scm
{
  int type;
  int car;
  int cdr;
};

char g_arena[84];
#if __MESC__
struct scm *g_cells = g_arena;
#else
struct scm *g_cells = (struct scm *) g_arena;
#endif
char *g_chars = g_arena;

int g = 48;
int
get ()
{
  int i = g;
  g++;
  return i;
}

int
main ()
{
  char *p = (char *) g_chars;
  int i = 0;

  oputs ("\n: ");
  oputs ("t: read 0123456789\nt: ");
  int c = get ();
  while (i < 10)
    {
      *p++ = c;
      putchar (c);
      c = get ();
      i++;
    }
  oputs ("\n");
  if (strcmp (g_chars, "0123456789"))
    return 1;

  oputs ("t: fdungetc ('A') == getchar ()\n");
  fdungetc ('A', STDIN);
  if (getchar () != 'A')
    return 2;
  oputs ("t: fdungetc (0)\n");
  fdungetc (0, STDIN);
  if (getchar () != 0)
    return 3;

  oputs ("t: i == 'm'\n");
  char m = 0x1122336d;
  i = m;
  if (i != 'm')
    return 4;

  return 0;
}

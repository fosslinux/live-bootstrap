/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <mes/lib-mini.h>

char g_arena[10];
char *g_chars = g_arena;

int
main ()
{
  int i = 0;
  char c = 'C';
  char *p = "mes";
  char *x = g_arena;
  char *y = g_chars;

  oputs ("\n");
  oputs ("t: p[0] != 'm'\n");
  if (p[0] != 'm')
    return p[0];

  oputs ("t: p[i] != 't'\n");
  if (p[i] != 'm')
    return p[i];

  oputs ("t: *g_chars != 'A'\n");
  g_arena[0] = 'A';
  if (*g_chars != 'A')
    return 1;

  oputs ("t: *x != 'A'\n");
  if (*x != 'A')
    return 1;

  oputs ("t: *y != 'A'\n");
  if (*y != 'A')
    return 1;

  oputs ("t: *x != 'Q'\n");
  g_chars[0] = 'Q';
  if (*x != 'Q')
    return 1;

  oputs ("t: *x++ != 'C'\n");
  *x++ = c;
  if (*g_chars != 'C')
    return 1;

  oputs ("t: *g_chars == 'B'\n");
  g_arena[0] = 'B';
  if (*g_chars == 'B')
    goto ok1;
  return 1;
ok1:

  oputs ("t: *x == 'B'\n");
  x = g_arena;
  if (*x == 'B')
    goto ok2;
  return 1;
ok2:

  oputs ("t: *y == 'B'\n");
  y = g_chars;
  if (*y == 'B')
    goto ok3;
  return 1;
ok3:

  oputs ("t: *x == 'R'\n");
  g_chars[0] = 'R';
  if (*x == 'R')
    goto ok4;
  return 1;
ok4:

  oputs ("t: *x++ == 'C'\n");
  *x++ = c;
  if (*g_chars == 'C')
    goto ok5;
  return 1;
ok5:

  return 0;
}

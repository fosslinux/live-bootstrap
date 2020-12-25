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

#include <mes/lib.h>

#include <stdio.h>
#include <string.h>

struct foo
{
  int *bar;
};

struct foo f;

int bla[6] = { 0, 0, 11223344, 55667788, 0, 0 };
int g_c[2] = { 101, 111 };

int
main ()
{
  f.bar = bla;
  struct foo *pf = &f;
  int *b = pf->bar;
  if (bla[2] != 11223344)
    return 1;
  if (bla[3] != 55667788)
    return 2;
  if (b[2] != 11223344)
    return 3;
  if (b[3] != 55667788)
    return 4;

  eputs ("g_c[0]=");
  eputs (itoa (g_c[0]));
  eputs ("\n");
  eputs ("g_c[1]=");
  eputs (itoa (g_c[1]));
  eputs ("\n");

  memcpy (&b[2], g_c, 2 * sizeof (int));
  eputs ("b[2]:");
  eputs (itoa (b[2]));
  eputs ("\n");

  if (b[2] != 101)
    return 5;
  eputs ("b[3]:");
  eputs (itoa (b[3]));
  eputs ("\n");
  if (b[3] != 111)
    return 6;

  int c[2] = { 201, 211 };
  eputs ("c[0]=");
  eputs (itoa (c[0]));
  eputs ("\n");
  eputs ("c[1]=");
  eputs (itoa (c[1]));
  eputs ("\n");

  memcpy (&b[4], c, 2 * sizeof (int));

  eputs ("b[4]:");
  eputs (itoa (b[4]));
  eputs ("\n");

  if (b[4] != 201)
    return 7;
  eputs ("b[5]:");
  eputs (itoa (b[5]));
  eputs ("\n");
  if (b[5] != 211)
    return 8;

  return 0;
}

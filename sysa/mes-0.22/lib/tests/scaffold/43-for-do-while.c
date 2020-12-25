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

int
main ()
{
  int i = 0;
  int f = 0;
  int t = 1;
  int one = t;
  char *p = "mes";

  oputs ("\n");

  oputs ("t: for (i=1; i<5; ++i)\n");
  for (i = 1; i < 5; ++i)
    ;
  if (i != 5)
    return i;

  oputs ("t: while (i<3) i++\n");
  i = 1;
  while (i < 3)
    i++;
  if (i != 3)
    return i;

  oputs ("t: do i-- while (i>0)\n");
  do
    i--;
  while (i > 0)
  ;
  if (i != 0)
    return 1;

  oputs ("t: while (1) break;\n");
  while (1)
    break;

  oputs ("t: while (1) ... break;\n");
  while (1)
    {
      f = 0;
      break;
    }

  oputs ("t: while (1) {while (1) break;break;}\n");
  while (1)
    {
      while (1)
        break;
      break;
    }

  oputs ("t: while () {continue;...}\n");
  while (one--)
    {
      continue;
      one = 1;
    }
  one += 2;

  oputs ("t: while (1) { goto label; };\n");
  while (1)
    {
      goto ok1;
    }
ok1:

  return 0;
}

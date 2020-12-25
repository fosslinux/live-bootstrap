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

enum type_t
{ TCHAR, TCLOSURE, TCONTINUATION, TFUNCTION, TKEYWORD, TMACRO, TNUMBER, TPAIR, TREF, TSPECIAL, TSTRING,
    TSYMBOL, TVALUES, TVECTOR, TBROKEN_HEART };

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

int
identity (int i)
{
  return i;
}

int
main ()
{
  int i = 0;
  int f = 0;
  int t = 1;
  int one = t;
  char *p = "mes";

  oputs ("\n");
  oputs ("t: if (strlen (\"\"))\n");
  if (strlen (""))
    return 1;

  oputs ("t: if (strlen (p) != 3)\n");
  if (strlen (p) != 3)
    return 2;

  oputs ("t: if (!strlen (\".\"))\n");
  if (!strlen ("."))
    return 3;

  oputs ("t: identity (p[i]) != 'm'\n");
  if (identity (p[i]) != 'm')
    return identity (p[i]);

  oputs ("t: inc (0)\n");
  if (inc (0) != 1)
    return 4;

  oputs ("t: inc (inc (0))\n");
  if (inc (inc (0)) != 2)
    return 5;

  oputs ("t: inc (inc (inc (0)))\n");
  if (inc (inc (inc (0))) != 3)
    return 6;

  oputs ("t: add (1, 2)\n");
  if (add (1, 2) != 3)
    return 7;

  // broken x86, x86_64
  oputs ("t: add (inc (0), inc (1))\n");
  if (add (inc (0), inc (1)) != 3)
    return 8;
  // end broken x86, x86_64

  oputs ("t: add (TSTRING, 3)\n");
  if (add (TSTRING, 3) != 13)
    return 9;

  // broken x86_64
  oputs ("t: add (inc (inc (0)), inc (inc (1)))\n");
  if (add (inc (inc (0)), inc (inc (1))) != 5)
    return 10;
  // end broken x86_64

  oputs ("t: if (strlen (\".\"))\n");
  if (strlen ("."))
    goto ok1;
  return 11;
ok1:

  oputs ("t: if (strlen (p) == 3)\n");
  if (strlen (p) == 3)
    goto ok2;
  return 12;
ok2:

  return 0;
}

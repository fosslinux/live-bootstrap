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
isid (char c)
{
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

int
main (int c)
{
  int f = 0;
  int t = 1;
  int one = t;

  oputs ("\n");
  oputs ("t: if (f)\n");
  if (f)
    return 1;

  oputs ("t: if (one != 1)\n");
  if (one != 1)
    return 2;

  oputs ("t: if (1 != one)\n");
  if (1 != one)
    return 3;

  oputs ("t: if (one > 1)\n");
  if (one > 1)
    return 4;

  oputs ("t: if (one < 0)\n");
  if (one < 0)
    return 5;

  oputs ("t: if (one <= 0)\n");
  if (one <= 0)
    return 6;

  oputs ("t: if (one >= 2)\n");
  if (one >= 2)
    return 7;

  oputs ("t: if (!1)\n");
  if (!1)
    return 8;

  oputs ("t: if (one == 0)\n");
  if (one == 0)
    return 9;

  oputs ("t: if (f != 0)\n");
  if (one != 1)
    return 10;

  oputs ("t: if (1)\n");
  if (1)
    goto ok0;

  return 111;
ok0:

  oputs ("t: if (0); return 1; else;\n");
  if (0)
    return 12;
  else
    goto ok1;
ok1:

  oputs ("t: if (t)\n");
  if (t)
    goto ok2;

  return 13;
ok2:

  oputs ("t: if (one > 0)\n");
  if (one > 0)
    goto ok3;

  return 14;
ok3:

  oputs ("t: if (one < 2)\n");
  if (one < 2)
    goto ok4;

  return 15;
ok4:

  oputs ("t: if (one >= 0)\n");
  if (one >= 0)
    goto ok5;

  return 16;
ok5:

  oputs ("t: if (one >= 1)\n");
  if (one >= 0)
    goto ok6;

  return 17;
ok6:

  oputs ("t: if (one <= 2)\n");
  if (one <= 2)
    goto ok7;

  return 18;
ok7:

  oputs ("t: if (one <= 1)\n");
  if (one <= 1)
    goto ok8;

  return 19;
ok8:

  oputs ("t: if (!0)\n");
  if (!0)
    goto ok9;

  return 20;
ok9:

  oputs ("t: if (one == 1)\n");
  if (one == 1)
    goto ok10;

  return 21;
ok10:

  oputs ("t: if (one != 0)\n");
  if (one != 0)
    goto ok11;

  return 22;
ok11:
  ;

  int m1 = -1;
  int i;

  oputs ("t: i = one > 0\n");
  i = one > 0;
  if (!i)
    return 23;

  oputs ("t: i = one >= 1\n");
  i = one >= 1;
  if (!i)
    return 24;

  oputs ("t: i = one < 2\n");
  i = one < 2;
  if (!i)
    return 25;

  oputs ("t: i = one <= 1\n");
  i = one <= 1;
  if (!i)
    return 26;


  oputs ("t: i = 0 > one\n");
  i = 0 > one;
  if (i)
    return 27;

  oputs ("t: i = 0 >= one\n");
  i = 0 >= one;
  if (i)
    return 28;

  oputs ("t: i = 1 < one \n");
  i = 1 < one;
  if (i)
    return 29;

  oputs ("t: i = 2 <= one\n");
  i = 2 <= one;
  if (i)
    return 30;


  oputs ("t: i = m1 > -2\n");
  i = m1 > -2;
  if (!i)
    return 31;

  oputs ("t: i = m1 >= -1\n");
  i = m1 >= -1;
  if (!i)
    return 32;

  oputs ("t: i = m1 < 0\n");
  i = m1 < 0;
  if (!i)
    return 33;

  oputs ("t: i = m1 <= -1\n");
  i = m1 <= -1;
  if (!i)
    return 34;


  oputs ("t: i = -1 > m1\n");
  i = -1 > m1;
  if (i)
    return 35;

  oputs ("t: i = -2 >= m1\n");
  i = -2 >= m1;
  if (i)
    return 36;

  oputs ("t: i = -1 < m1 \n");
  i = -1 < m1;
  if (i)
    return 37;

  oputs ("t: i = -2 <= m1\n");
  i = 0 <= m1;
  if (i)
    return 38;


  oputs ("t: isid (0)\n");
  if (isid (0))
    return 39;

  oputs ("t: isid (6)\n");

  if (isid (6))
    return 40;

  oputs ("t: isid (a)\n");
  if (isid ('a') != 1)
    return 41;

  oputs ("t: isid ( )\n");
  if (isid (' '))
    return 42;

  return 0;
}

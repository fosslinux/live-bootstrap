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

#include <limits.h>
#include <stdio.h>
#include <string.h>

#include <mes/lib.h>

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
main ()
{
  int i;

  oputs ("\n");
  oputs ("t: 0 < 0\n");
  if (0 < 0)
    return 1;

  oputs ("t: 2 < 1\n");
  if (2 < 1)
    return 2;

  oputs ("t: -1 < -2\n");
  if (-1 < -2)
    return 3;

  oputs ("t: 0 < -1\n");
  if (0 < -1)
    return 4;

  oputs ("t: 0 > 0\n");
  if (0 > 0)
    return 5;

  oputs ("t: 1 > 2\n");
  if (1 > 2)
    return 6;

  oputs ("t: -2 > -1\n");
  if (-2 > -1)
    return 7;

  oputs ("t: -1 > 0\n");
  if (-1 > 0)
    return 9;

  oputs ("t: 1 == inc (0)\n");
  if (1 == inc (0))
    goto ok0;
  return 10;
ok0:

  oputs ("t: 0 < inc (0)\n");
  if (0 < inc (0))
    goto ok1;
  return 11;
ok1:

  oputs ("t: inc (0) + 2 != 3\n");
  if (inc (0) + inc (1) != 3)
    return 12;

  oputs ("t: 4/2=");
  i = 4 / 2;
  if (i != 2)
    return 13;
  i += 48;
  putchar (i);
  oputs ("\n");

  oputs ("t: 3*4=\n");
  i = 3 * 4;
  if (i != 12)
    return 14;

  oputs ("t: i /= 4\n");
  i /= 4;
  if (i != 3)
    return 15;

  oputs ("t: i *= 4\n");
  i *= 4;
  if (i != 12)
    return 16;

  oputs ("t: 1 << 3\n");
  if (1 << 3 != 8)
    return 1 << 3;

  oputs ("t: 3 << 4\n");
  if (3 << 4 != 48)
    return 3 << 4;

  oputs ("t: 48 >> 3\n");
  if (48 >> 4 != 3)
    return 48 >> 4;

  oputs ("t: 10 >> 1\n");
  if (10 >> 1 != 5)
    return 10 >> 1;

  oputs ("t: 1 | 4\n");
  if ((1 | 4) != 5)
    return 1 | 4;

  i = -3;
  oputs ("t: -i\n");
  if (-i != 3)
    return 22;

  oputs ("t: -1 + 2\n");
  if (-1 + 2 != 1)
    return 23;

  oputs ("t: 1 & 3\n");
  if ((1 & 3) != 1)
    return 24;

  oputs ("t: ~0\n");
  if (~0 != -1)
    return 25;

  oputs ("t: 1 | 3\n");
  if ((1 | 2) != 3)
    return 26;

  oputs ("t: ^ 1 \n");
  if ((1 ^ 3) != 2)
    return 27;

  oputs ("t: 3 == 3\n");
  if ((3 == 3) != 1)
    return 28;

  oputs ("t: 3 != 3\n");
  if ((3 != 3) != 0)
    return 29;

  oputs ("t: 011 == 15\n");
  if (011 != 9)
    return 30;

  oputs ("t: 0b11 == 3\n");
  if (0b11 != 3)
    return 31;

  oputs ("t: 0x11 == 3\n");
  if (0x11 != 17)
    return 32;

  oputs ("t: i = INT_MAX\n");
  i = INT_MAX;

  if (strcmp ("2147483647", itoa (i)))
    return 33;

  oputs ("t: i = 2147483646\n");
  i = 2147483646;

  oputs ("t: i++\n");
  i++;

  oputs ("t: i = INT_MIN\n");
  i = INT_MIN;

  if (strcmp ("-2147483648", itoa (i)))
    return 34;

  oputs ("t: i = -2147483647\n");
  i = -2147483647;

  oputs ("t: i--\n");
  i--;

  return 0;
}

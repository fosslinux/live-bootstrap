/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

int
main ()
{
  {
    int i = 0;
    if (i < -1)
      return 1;
  }

  {
    unsigned u = 0;
    if (-1 < u)
      return 2;
  }

  {
    int i = INT_MAX + 2;
    if (INT_MAX < i)
      return 3;
  }

  {
    unsigned u = INT_MAX + 2;
    if (u < INT_MAX)
      return 4;
  }

  {
    int i = -1;
    if (-1 > 0)
      return 5;
  }

  {
    unsigned u = INT_MAX + 2;
    if (INT_MAX > u)
      return 6;
  }

  {
    int i = INT_MAX + 2;
    if (i > 0)
      return 7;
  }

  {
    unsigned u = 0;
    if (u > -1)
      return 8;
  }


  {
    int i = 0;
    if (i <= -1)
      return 9;
  }

  {
    unsigned u = 0;
    if (-1 <= u)
      return 10;
  }

  {
    int i = INT_MAX + 2;
    if (INT_MAX <= i)
      return 11;
  }

  {
    unsigned u = INT_MAX + 2;
    if (u <= INT_MAX)
      return 12;
  }

  {
    int i = -1;
    if (-1 >= 0)
      return 13;
  }

  {
    unsigned u = INT_MAX + 2;
    if (INT_MAX >= u)
      return 14;
  }

  {
    int i = INT_MAX + 2;
    if (i >= 0)
      return 15;
  }

  {
    unsigned u = 0;
    if (u >= -1)
      return 16;
  }

  return 0;
}

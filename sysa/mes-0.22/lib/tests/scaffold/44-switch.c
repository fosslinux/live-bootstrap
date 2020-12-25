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
swits (int c)
{
  int x = -1;

  switch (c)
    {
    case TCHAR:
      {
        oputs ("TCHAR\n");
        goto next;
      }
    case 1:
      {
        oputs ("1\n");
        goto next;
      }
    case 2:
      {
        oputs ("2\n");
        goto next;
      }
    default:
      {
        oputs ("default\n");
        goto next;
      }
    }

  return 1;
next:
  switch (c)
    {
    case 0:
      {
        oputs ("0\n");
        x = 0;
        c = 34;
        break;
      }
    case 5:
    case 4:
    case 3:
    case 2:
    case -1:
    case 1:
      oputs ("5..1, -1\n");
      x = 1;
      break;
    default:
      oputs ("default\n");
      x = 2;
      x = 2;
      break;
    }
  return x;
}

int
default_first (int c)
{
  int a;
  switch (c)
    {
    here:
    default:
      a = 1;
      {
      }
      a = 2;
      return a;
    there:
    case 0:
      ;
      {
      }
      return 0;
    }
  return -1;
}

int
main ()
{
  oputs ("\n");
  oputs ("t: switch 0\n");
  int i = swits (0);
  if (i != 0)
    return i;

  oputs ("t: switch 1\n");
  if (swits (1) != 1)
    return 10;

  oputs ("t: switch -1\n");
  if (swits (-1) != 1)
    return 11;

  oputs ("t: switch -1\n");
  if (swits (-2) != 2)
    return 12;

  if (default_first (1) != 2)
    return 13;

  if (default_first (0) != 0)
    return 14;

  i = 15;
  switch (i)
    {
    case 0:
    case 1:
    case 2:
    case 3:
    case 4:
      i = 15;
      break;
    }
  if (i != 15)
    return 15;

  i = 16;
  switch (i)
    {
    case 1:
    default:
    case 0:
      i = 0;
      break;
    }

  if (i != 0)
    return 16;

  i = 2;
  switch (i)
    {
    default:
    case 0:
      i = 17;
      break;
    case 2:
      i = 0;
    }

  return i;
}

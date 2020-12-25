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

char global_c = -1;
struct foo
{
  char type;
};
int
main ()
{
  {
    char c = -1;
    int i = c;
    if (i != -1)
      return 1;
  }

  {
    int i = global_c;
    if (i != -1)
      return 2;
  }

  {
    char c = -1;
    int ints[2] = { c, 0 };
    if (ints[0] != -1)
      return 3;
  }

  {
    char c = -1;
    int i = c;
    if (i != -1)
      return 4;
  }

  {
    char c = -1;
    int i = c;
    if (i != -1)
      return 5;
  }

  {
    char a[2] = { -1, -129 };
    int i = a[0];
    if (i != -1)
      return 6;
    if (a[0] != -1)
      return 7;
  }

  {
    struct foo f = { -1 };
    int i = f.type;
    if (i != -1)
      return 8;

    struct foo *g = &f;
    i = g->type;
    if (i != -1)
      return 9;
  }

  {
    char c = -1;
    char *p = &c;
    int i = *p;
    if (i != -1)
      return 10;
  }

  {
    int i = -129;
    i = (char) i;
    if (i != 127)
      return 11;
  }

  {
    unsigned char b = -129;
    int i = b;
    if (i != 127)
      return 12;
  }

  return 0;
}

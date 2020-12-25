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

struct bits
{
  int one:1, two:1, four:1, eightsixteen:2;
};

union foo
{
  struct bits b;
  int i;
};

int
main ()
{
  union foo f;
  f.i = 1;
  f.b.one = 1;
  if (f.i != 1)
    return 1;
  f.b.two = 1;
  if (f.i != 3)
    return f.i;
  f.b.four = 1;
  if (f.i != 7)
    return 3;
  f.b.eightsixteen = 3;
  if (f.i != 31)
    return 4;

  f.i = 1;
  f.b.one = 0;
  if (f.i)
    return 5;
  f.i = 24;
  f.b.eightsixteen = 0;
  if (f.i)
    return 6;
  f.i = 8;
  f.b.eightsixteen = 2;
  if (f.i != 16)
    return 7;

  return 0;
}

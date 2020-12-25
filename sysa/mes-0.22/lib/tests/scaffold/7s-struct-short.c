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


struct foo
{
  char c;
  short bar;
  short baz;
};

struct bar
{
  char bar;
};

struct foo global_f = { 0, 11, 22 };
struct bar global_b = { 11 };

int i = 0x11223344;

struct foo foes[2] = { {0, 1, 2}, {0, 3, 4} };

int
main ()
{
  if (global_f.bar != 11)
    return 1;

  if (global_f.baz != 22)
    return 2;

  struct foo f = { 0, 44, 55 };

  if (f.bar != 44)
    return 3;

  if (f.baz != 55)
    return 4;

  if (global_b.bar != 11)
    return 5;

  if (foes[0].bar != 1)
    return 6;

  if (foes[0].baz != 2)
    return foes[0].baz;

  if (foes[1].bar != 3)
    return 8;

  if (foes[1].baz != 4)
    return 9;

  return 0;
}

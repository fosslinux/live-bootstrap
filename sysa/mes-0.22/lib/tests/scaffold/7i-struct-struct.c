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

struct s
{
  int bar;
  int baz;
  int bla[2];
};

struct foo
{
  int bar;
  struct s s;
};

struct anon
{
  struct
  {
    int bar;
    int baz;
  };
};


int
main ()
{
  struct foo f = { 0, 1, 2 };
  f.s.baz = 2;
  oputs ("f.s.bar=");
  oputs (itoa (f.s.bar));
  oputs ("\n");
  if (f.s.bar != 1)
    return 1;
  oputs ("f.s.baz=");
  oputs (itoa (f.s.baz));
  oputs ("\n");
  if (f.s.baz != 2)
    return 2;

  struct anon a = { 3, 4 };
  a.baz = 4;
  oputs ("a.bar=");
  oputs (itoa (a.bar));
  oputs ("\n");
  if (a.bar != 3)
    return 3;
  oputs ("a.baz=");
  oputs (itoa (a.baz));
  oputs ("\n");
  if (a.baz != 4)
    return 4;

  return 0;
}

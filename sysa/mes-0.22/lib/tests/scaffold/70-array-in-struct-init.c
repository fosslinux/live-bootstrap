/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

struct foo
{
  int field;
  int array[3];
};

struct foo foes[] =
  {
   {10, {11, 12, 13}},
   {20, {21, 22}},
   {30, {31}},
   {40, {41}},
   {0}
  };

int
main ()
{
  for (struct foo *p = foes; p->field; p++)
    {
      oputs ("{");
      oputs (itoa (p->field)); oputs (",");
      oputs ("{");
      oputs (itoa (p->array[0])); oputs (",");
      oputs (itoa (p->array[1])); oputs (",");
      oputs (itoa (p->array[2]));
      oputs ("}");
      oputs ("},\n");
    }
  oputs ("{0}\n");
  oputs ("};\n");

  return 0;
}

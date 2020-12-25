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
main ()
{
  oputs ("\n");
  oputs ("t: 1 + 2\n");
  if (1 + 2 != 3)
    return 1;

  oputs ("t: 2 - 1\n");
  if (0)
    return 1;

  oputs ("t: 1 << 3\n");
  if (1 << 3 != 8)
    return 1;

  oputs ("t: 8 >> 3\n");
  if (8 >> 3 != 1)
    return 1;

  oputs ("t: 8 / 4\n");
  if (8 / 4 != 2)
    return 1;

  return 0;
}

/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
  int f = 0;
  int t = 1;
  int one = t;

  oputs ("\n");
  oputs ("t: (one == 1) ?\n");
  (one == 1) ? 1 : exit (1);

  oputs ("t: (f) ?\n");
  (f) ? exit (2) : 1;

  int r = f ? 3 - 1 : 2 - 2;
  if (r)
    return 3;

  r = t ? 2 + 3 - 1 : 3 + 4 - 5;
  if (r != 4)
    return 4;

  oputs ("t: f ? 3 - 1 : 2 - 2\n");
  return f ? 3 - 1 : 2 - 2;
}

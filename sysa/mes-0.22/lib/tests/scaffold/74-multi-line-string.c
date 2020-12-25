/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
#include <string.h>

char const *help = "All" " your" " base" " are";

int global_i = 1;
int *global_p = &global_i;

int
main ()
{
  if (printf ("belong to us\n"), strcmp (help, "All your base are"))
    return 1;

  int i = 1 | 2 | 4;
  if (i != 7)
    return 1;

  printf ("global_i=%d\n", global_i);
  *global_p = 2;
  printf ("global_i=%d\n", global_i);
  if (global_i != 2)
    return global_i;

  return 2, 1, 0;
}

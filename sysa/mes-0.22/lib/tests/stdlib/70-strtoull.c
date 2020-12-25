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
#include <mes/lib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int
main ()
{
  char *p = "42foo\n";
  int n = abtol (&p, 0);
  if (n != 42)
    return 1;
  eputs (p);
  if (strcmp (p, "foo\n"))
    return 2;

  p = "2azar\n";
  n = strtoull (p, (char **) &p, 16);
  if (n != 42)
    return 3;
  eputs (p);
  if (strcmp (p, "zar\n"))
    return 4;

  return 0;
}

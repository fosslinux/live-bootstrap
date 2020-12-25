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

#include <mes/lib.h>
#include <stdlib.h>

int
main ()
{
  eputs ("0x12\n");
  if (strtol ("0x12", 0, 0) != 18)
    1;

  eputs ("012\n");
  if (strtol ("012", 0, 0) != 10)
    2;

  eputs ("-1\n");
  if (strtol ("-1", 0, 0) != -1)
    3;

  eputs ("-1\n");
  if (strtoul ("-1", 0, 0) != -1)
    4;

  char *p = "16";
  int n = strtol (p, (char **) &p, 0);
  eputs ("p=");
  eputs (p);
  eputs ("\n");
  if (*p != 0)
    return 5;

  p = "0x12";
  n = strtol (p, (char **) &p, 0);
  eputs ("p=");
  eputs (p);
  eputs ("\n");
  if (*p != 0)
    return 5;


  return 0;
}

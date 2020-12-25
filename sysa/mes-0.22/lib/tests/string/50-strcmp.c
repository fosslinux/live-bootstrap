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

#include <string.h>

int
main ()
{
  char *p = "mes";

  oputs ("\n");
  oputs ("t: if (strcmp (p, \"foo\"))\n");
  if (!strcmp (p, "foo"))
    return 1;

  oputs ("t: if (strcmp (p, \"t.c\\n\"))\n");
  if (strcmp (p, "mes"))
    return 2;

  oputs ("t: if (!strcmp (p, \"t.c\\n\"))\n");
  if (!strcmp (p, "mes"))
    goto ok1;
  return 3;
ok1:

  oputs ("t: if (strcmp (p, \"foo\"))\n");
  if (strcmp (p, "foo"))
    goto ok2;
  return 4;
ok2:

  oputs ("t: itoa (33) == \"33\"\n");
  if (strcmp (itoa (33), "33"))
    return 5;

  oputs ("strcmp (itoa (-1), \"-1\")\n");
  if (strcmp (itoa (-1), "-1"))
    return 6;

  oputs ("strcmp (itoa (0), \"0\")\n");
  if (strcmp (itoa (0), "0"))
    return 7;

  oputs ("strcmp (itoa (1), \"1\")\n");
  if (strcmp (itoa (1), "1"))
    return 8;

  return 0;
}

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
#include <string.h>
#include <stdarg.h>

int
main ()
{
  int i = 42;
  char *s = "mes";
  char buf[20];

  printf ("i=%d\n", i);
  sprintf (buf, "i=%d\n", i);
  if (strcmp (buf, "i=42\n"))
    return 1;

  printf ("s=%s\n", s);
  sprintf (buf, "s=%s\n", s);
  if (strcmp (buf, "s=mes\n"))
    return 2;

  return 0;
}

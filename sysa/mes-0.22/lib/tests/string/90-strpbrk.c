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

#include <stdio.h>

int
main ()
{
  char buf[100];
  strcpy (buf, "foo");

  strcat (buf, " bar");
  eputs (buf);
  eputs ("\n");
  if (strcmp (buf, "foo bar"))
    return 1;

  strncat (buf, " bazzzz", 4);
  eputs (buf);
  eputs ("\n");
  if (strcmp (buf, "foo bar baz"))
    return 2;

  char *p = strpbrk ("hello, world", " \t\n,.;!?");
  if (strcmp (p, ", world"))
    return 3;
  eputs ("\n");

  if (strspn ("hello, world", "abcdefghijklmnopqrstuvwxyz") != 5)
    return 4;

  if (strcspn ("hello, world", " \t\n,.;!?") != 5)
    return 5;


  return 0;
}

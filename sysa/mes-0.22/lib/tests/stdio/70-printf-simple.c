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
  char *s = "mes";
  char c = 'm';
  int i = 3;
  char buf[20];

  printf ("c=%c\n", c);
  sprintf (buf, "c=%c\n", c);
  if (strcmp (buf, "c=m\n"))
    return 1;

  if (i != 3)
    return 2;

  printf ("i=%d\n", i);
  sprintf (buf, "i=%d\n", i);
  if (strcmp (buf, "i=3\n"))
    return 3;

  printf ("s=%s\n", s);
  sprintf (buf, "s=%s\n", s);
  if (strcmp (buf, "s=mes\n"))
    return 4;

  sprintf (buf, "%u", -1);
  eputs ("buf=");
  eputs (buf);
  eputs ("\n");

#if __i386__
  if (strcmp (buf, "4294967295"))
    return 5;
#elif __x86_64__
  if (strcmp (buf, "18446744073709551615"))
    return 6;
#endif

  sprintf (buf, ">>%o<<\n", 12);
  eputs ("buf=");
  eputs (buf);
  if (strcmp (buf, ">>14<<\n"))
    return 7;

  sprintf (buf, ">>%x<<\n", 12);
  eputs ("buf=");
  eputs (buf);
  if (strcmp (buf, ">>c<<\n"))
    return 8;

  sprintf (buf, ">>%X<<\n", 12);
  eputs ("buf=");
  eputs (buf);
  if (strcmp (buf, ">>C<<\n"))
    return 9;

  return 0;
}

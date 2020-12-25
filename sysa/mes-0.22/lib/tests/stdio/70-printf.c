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
    return 15;
  printf ("i=%d\n", i);
  sprintf (buf, "i=%d\n", i);
  if (strcmp (buf, "i=3\n"))
    return 2;

  printf ("s=%s\n", s);
  sprintf (buf, "s=%s\n", s);
  if (strcmp (buf, "s=mes\n"))
    return 3;

  sprintf (buf, ">%3d<", 11);
  eputs (buf);
  eputs ("\n");
  if (strcmp (buf, "> 11<"))
    return 4;

  sprintf (buf, ">%03d<", 22);
  eputs (buf);
  eputs ("\n");
  if (strcmp (buf, ">022<"))
    return 5;

  sprintf (buf, ">%-10d<", 33);
  eputs (buf);
  eputs ("\n");
  if (strcmp (buf, ">33        <"))
    return 6;

  sprintf (buf, ">%0d<", 44);
  eputs (buf);
  eputs ("\n");
  if (strcmp (buf, ">44<"))
    return 7;

  printf (">>%.*s<<\n", 5, "hello, world");
  printf (">>%.*s<<\n", 20, "hello, world");

  printf (">>%.*s<<\n", 10, "foo");
  printf (">>%*s<<\n", 10, "bar");
  printf (">>%-*s<<\n", 10, "baz");

  sprintf (buf, "%ld", 42);
  eputs ("buf=");
  eputs (buf);
  eputs ("\n");
  if (strcmp (buf, "42"))
    return 8;

  sprintf (buf, "%u", -1);
  eputs ("buf=");
  eputs (buf);
  eputs ("\n");

#if __i386__
  if (strcmp (buf, "4294967295"))
    return 9;
#elif __x86_64__
  if (strcmp (buf, "18446744073709551615"))
    return 9;
#endif

  sprintf (buf, ">>%.5s<<\n", "hello, world");
  eputs ("buf=");
  eputs (buf);
  if (strcmp (buf, ">>hello<<\n"))
    return 10;

  sprintf (buf, ">>%.*s<<\n", 5, "hello, world");
  eputs ("buf=");
  eputs (buf);
  if (strcmp (buf, ">>hello<<\n"))
    return 11;

  sprintf (buf, ">>%.*s<<\n", 20, "hello, world");
  eputs ("buf=");
  eputs (buf);
  if (strcmp (buf, ">>hello, world<<\n"))
    return 12;

  sprintf (buf, ">>%.*s<<\n", 10, "foo");
  eputs ("buf=");
  eputs (buf);
  if (strcmp (buf, ">>foo<<\n"))
    return 13;

  sprintf (buf, ">>%*s<<\n", 10, "bar");
  eputs ("buf=");
  eputs (buf);
  if (strcmp (buf, ">>       bar<<\n"))
    return 14;

  sprintf (buf, ">>%-*s<<\n", 10, "baz");
  eputs ("buf=");
  eputs (buf);
  if (strcmp (buf, ">>baz       <<\n"))
    return 15;

  sprintf (buf, ">>%ld<<\n", 12);
  eputs ("buf=");
  eputs (buf);
  if (strcmp (buf, ">>12<<\n"))
    return 16;

  sprintf (buf, ">>%o<<\n", 12);
  eputs ("buf=");
  eputs (buf);
  if (strcmp (buf, ">>14<<\n"))
    return 17;

  sprintf (buf, ">>%x<<\n", 12);
  eputs ("buf=");
  eputs (buf);
  if (strcmp (buf, ">>c<<\n"))
    return 18;

  sprintf (buf, ">>%X<<\n", 12);
  eputs ("buf=");
  eputs (buf);
  if (strcmp (buf, ">>C<<\n"))
    return 19;

  int n;
#if !__x86_64__
  fprintf (stderr, "foo bar\n%n", &n);
  if (n != 8)
    return 20;
#endif

  sprintf (buf, "foo%nbar\n", &n);
  eputs ("buf=");
  eputs (buf);
  eputs ("\n");
  if (strcmp (buf, "foobar\n"))
    return 21;
  if (n != 3)
    return 22;

#if !__x86_64__
  fprintf (stdout, "%12.8d\n", 12345);
#endif

  sprintf (buf, "%12.8d\n", 12345);
  eputs ("buf=");
  eputs (buf);
  eputs ("\n");
  if (strcmp (buf, "    00012345\n"))
    return 23;

  return 0;
}

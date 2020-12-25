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
#include <stdlib.h>
#include <string.h>

char *env[] = { "foo", "bar", "baz", 0 };

#if 0                           //!SYSTEM_LIBC
#define getenv xgetenv

char *
getenv (char const *s)
{
  eputs ("getenv\n");
  char **p = environ;
  int length = strlen (s);
  eputs ("getenv length=");
  eputs (itoa (length));
  eputs ("\n");
  while (*p)
    {
      eputs ("*p=");
      eputs (*p);
      eputs ("\n");;
      eputs (" p=");
      eputs (itoa ((long) p));
      eputs ("\n");
      if (!strncmp (s, *p, length) && *(*p + length) == '=')
        return (*p + length + 1);
      p++;
    }
  return 0;
}
#endif

int
test (char **e)
{
  int i = 0;

  oputs ("\n");
  oputs ("a[i] = i-1\n");
  int a[3];
  for (int i = 0; i < 3; i++)
    a[i] = i - 1;
  for (int i = 0; i < 3; i++)
    if (a[i] != i - 1)
      return 1;

  oputs ("env [");
  oputs (itoa ((long) env));
  oputs ("]\n");

  oputs ("e [");
  oputs (itoa ((int) e));
  oputs ("]\n");

  oputs ("env [0] == \"foo\"\n");
  if (strcmp (env[0], "foo"))
    return 2;

  oputs ("env [1] == \"bar\"\n");
  if (strcmp (env[1], "bar"))
    return 3;

  oputs ("t: **p in *env[]\n");

  char **pp = env;
  while (*pp)
    {
      oputs ("pp [");
      oputs (itoa ((int) pp));
      oputs ("]: ");
      if (*pp)
        oputs (*pp);
      oputs ("\n");
      pp++;
      i++;
    }
  if (i != 3)
    return i;

  pp = env;
  oputs ("t: *pp++ == \"foo\"\n");
  if (strcmp (*pp++, "foo"))
    return 4;

  oputs ("t: *pp++ == \"bar\"\n");
  if (strcmp (*pp++, "bar"))
    return 5;

  char *buf = "hello";
  oputs ("t: buf[0]\n");
  if (buf[0] != 'h')
    return 6;

  oputs ("t: buf + 1\n");
  if (*(buf + 1) != 'e')
    return 7;

  char **p = &buf;
  oputs ("t: **p\n");
  if (**p != 'h')
    return 8;

  oputs ("t: *(p + 1)\n");
  if (*(*p + 1) != 'e')
    return 9;

  oputs ("t: getenv ()\n");
  if (!getenv ("PATH"))
    return 10;

  oputs ("t: setenv ()\n");
  if (setenv ("61-array", "yes", 1))
    return 11;

  oputs ("t: getenv2 ()\n");
  if (strcmp (getenv ("61-array"), "yes"))
    return 12;

  return 0;
}

int
main (int argc, char *argv[])
{
  return test (env);
}

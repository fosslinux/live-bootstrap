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
#include <stdlib.h>
#include <string.h>

struct symbol
{
  int len;
  char str[10];
  //int len;
};

struct symbol *list[2];
struct symbol s0;
struct symbol s1;
struct symbol **plist;

char *
find0 ()
{
  strcpy (s0.str, "foo");
  strcpy (s1.str, "bar");
  list[0] = &s0;
  list[1] = &s1;
  //return s0.str;
  //struct symbol *s = &s0;
  struct symbol *s = list[0];
  return s->str;
}

char *
find1 ()
{
  return list[1]->str;
}

char *
find2 ()
{
  plist = malloc (8);
  struct symbol *p0 = malloc (sizeof (struct symbol));
  struct symbol *p1 = malloc (sizeof (struct symbol));
  strcpy (p0->str, "pfoo");
  strcpy (p1->str, "pbar");
  plist[0] = p0;
  plist[1] = p1;
  int i = 3;
  return plist[i - 2]->str;
}

int
main ()
{
  char *s = find0 ();
  eputs (s);
  eputs ("\n");
  if (strcmp (s, "foo"))
    return 1;
  if (strcmp (list[0]->str, "foo"))
    return 2;

  s = find1 ();
  eputs (s);
  eputs ("\n");
  if (strcmp (s, "bar"))
    return 3;
  if (strcmp (list[1]->str, "bar"))
    return 4;

  s = find2 ();
  eputs (s);
  eputs ("\n");
  if (strcmp (s, "pbar"))
    return 5;

  list[1]->len = 2;
  if (list[1]->len != 2)
    return 6;

  return 0;
}

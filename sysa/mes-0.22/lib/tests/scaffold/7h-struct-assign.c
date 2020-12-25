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
#include <string.h>

struct string
{
  char *str;
  int len;
};

typedef struct biggie
{
  int a;
  int b;
  int c;
  char *str;
  int len;
} biggie;

struct other
{
  struct biggie big;
};

struct string g_t;

struct biggie tab[2];

int
main ()
{
  struct string s = { "hallo" };
  s.len = strlen (s.str);
  eputs (s.str);
  eputs ("\n");

  struct string t;
  t = s;

  eputs (t.str);
  eputs ("\n");
  if (t.len != s.len)
    return 1;
  if (strcmp (t.str, s.str))
    return 2;

  g_t = s;
  eputs (g_t.str);
  eputs ("\n");
  if (g_t.len != s.len)
    return 3;
  if (strcmp (g_t.str, s.str))
    return 4;

  struct biggie b;
  b.str = "hello";
  b.len = strlen (b.str);
  eputs (b.str);
  eputs ("\n");

  struct biggie tb;
  tb = b;
  eputs (tb.str);
  eputs ("\n");
  if (tb.len != b.len)
    return 5;
  if (strcmp (tb.str, b.str))
    return 6;

  b.str = "bye";
  b.len = strlen (b.str);
  eputs (b.str);
  eputs ("\n");
  //struct biggie *pb = &tb;
  biggie *pb = &tb;
  *pb = b;
  eputs (tb.str);
  eputs ("\n");
  if (tb.len != b.len)
    return 7;
  if (strcmp (tb.str, b.str))
    return 8;

  tb.str = "there";
  tb.len = strlen (tb.str);

  b = *pb;
  eputs (b.str);
  eputs ("\n");
  if (b.len != tb.len)
    return 9;
  if (strcmp (b.str, tb.str))
    return 10;

  char **x = &b.str;
  char *p;
  p = *x;

  struct other o;
  struct other *po = &o;
  po->big = b;
  eputs (o.big.str);
  eputs ("\n");
  if (o.big.len != b.len)
    return 13;
  if (strcmp (o.big.str, b.str))
    return 14;

  po->big = *pb;
  eputs (o.big.str);
  eputs ("\n");
  if (o.big.len != b.len)
    return 15;
  if (strcmp (o.big.str, b.str))
    return 16;

  b.str = "* = *";
  b.len = strlen (b.str);
  eputs (b.str);
  eputs ("\n");
  struct biggie *q = tab;
  pb = &b;
  *q++ = *pb;
  eputs (tab[0].str);
  eputs ("\n");
  if (tab[0].len != b.len)
    return 17;
  if (strcmp (tab[0].str, b.str))
    return 18;

  tab[1] = tab[0];
  eputs (tab[1].str);
  eputs ("\n");
  if (tab[1].len != b.len)
    return 19;
  if (strcmp (tab[1].str, b.str))
    return 20;

  tab[0].str = "burp";
  tab[0].len = strlen (tab[1].str);
  eputs (tab[0].str);
  eputs ("\n");
  b = tab[0];
  eputs (b.str);
  eputs ("\n");
  if (b.len != tab[0].len)
    return 21;
  if (strcmp (b.str, tab[0].str))
    return 22;

  tab[1] = b;
  eputs (tab[1].str);
  eputs ("\n");
  if (tab[1].len != b.len)
    return 23;
  if (strcmp (tab[1].str, b.str))
    return 24;

  return 0;
}

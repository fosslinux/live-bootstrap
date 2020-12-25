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

#include <string.h>
int one_two_three[3] = {
  1, 2, 3
};

char *foo_bar_baz[3] = {
  "foo", "bar", "baz"
};

char foo_bar_baz_haha[3][4] = {
  "foo", "bar", "baz"
};

char *foo = "foo";
char *bar = "bar";
char *baz = "baz";

char *foo_bar_baz_mwhuhahaha[3] = {
  &foo, &bar, &baz
};

int
main ()
{
  puts ("one:");
  puts (itoa (one_two_three[0]));
  puts ("\n");
  puts ("foo:");
  puts (foo_bar_baz[1]);
  puts ("\n");
  puts ("bar:");
  puts (foo_bar_baz_haha[2]);
  puts ("\n");
  char *p = foo_bar_baz_haha[2];
  puts ("baz:");
  puts (p);
  puts ("\n");
  return strcmp (foo_bar_baz[2], "baz");
}

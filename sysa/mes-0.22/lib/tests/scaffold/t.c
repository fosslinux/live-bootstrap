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


int puts (char const *);
#include <string.h>
char global_arena[10];
int global_i = 1;
int global_unitialized;
char *global_string = "foo";
char global_array[8] = "XXX";
char *global_chars = global_array;
typedef int SCM;
enum type_t
{ TCHAR };
char *env[] = { "foo", "bar", "baz", 0 };
char *list[2] = { "foo\n", "bar\n" };

struct foo
{
  int length;
  char *string;
};
struct foo g_f = { 3, "foo" };

struct foo *g_g = &g_f;
struct foo g_foes[2];
int g_foe;

struct anon
{
  struct
  {
    int bar;
    int baz;
  };
};
struct anion
{
  union
  {
    int foo;
    int bar;
  };
  union
  {
    int baz;
    int bla;
  };
};

struct here
{
  int and;
} there;

typedef int int_array_t[1];
int_array_t bar;

typedef struct foo *foo_pointer_t;
foo_pointer_t foep;

struct nest
{
  struct bar
  {
    int baz;
  } bar;
  int i;
  enum baz
  {
    bla
  } baz;
  enum
  {
    blub = 33,
  } blub;
};

int
test (struct foo *p)
{
  struct foo *g = &g_f;
  g[0].length = 0;
  p[0].length = 0;
}

int
next_main (int argc, char *argv[])
{
  return 0;
}

int
main (int argc, char *argv[])
{
  int i;
  int j = 1;
  int k, l = 1;
  if (j != 1)
    return 1;
  if (l != 1)
    return 2;
  if (global_i != 1)
    return 3;
  global_arena[1] = 0;
  if (global_i != 1)
    return 4;
  if (global_unitialized != 0)
    return 5;
  if (strcmp (global_string, "foo"))
    return 6;
  char *s = "bar";
  if (strcmp (s, "bar"))
    return 7;
  if (*global_array != 'X')
    return 8;
  if (*global_chars != 'X')
    return 9;
  SCM x = 0;
  if (x != 0)
    return 9;
  if (TCHAR != 0)
    return 11;
  if (strncmp (argv[0], "lib/test/scaffold", 5))
    return 12;
  if (strcmp (env[0], "foo"))
    return 13;
  if (strcmp (env[2], "baz"))
    return 14;
  if (env[3])
    return 15;
  if (g_f.length != 3)
    return 16;
  if (strcmp (g_f.string, "foo"))
    return 17;
  struct foo g = { 4, "baar" };
  if (g.length != 4)
    return 18;
  if (strcmp (g.string, "baar"))
    return 19;
  struct foo f = { 3, "foo" };
  g_foes[0] = f;
  g_foes[1] = f;
  if (g_foe)
    return 20;
  char *strings[] = { "one\n", "two\n", "three\n", 0 };
  char **p = strings;
  while (*p)
    eputs (*p++);
  if (strcmp (strings[1], "two\n"))
    return 21;
  p = list;
  struct anon a = { 3, 4 };
  eputs ("bar:");
  eputs (itoa (a.bar));
  eputs ("\n");
  eputs ("baz:");
  eputs (itoa (a.baz));
  eputs ("\n");
  if (a.bar != 3)
    return 22;
  if (a.baz != 4)
    return 23;

  struct anion u = { 3, 4 };
  eputs ("u.foo:");
  eputs (itoa (u.foo));
  eputs ("\n");
  eputs ("u.bla:");
  eputs (itoa (u.bla));
  eputs ("\n");
  if (u.foo != 3)
    return 24;
  if (u.bla != 4)
    return 25;

  struct nest n = { 0 };
  if (n.bar.baz)
    return 26;

  if (bla != 0)
    return 27;

  if (blub != 33)
    return 28;

  char buf[sizeof (g_f.string)];
  char buf1[sizeof (g_g->string)];

  int (*fun) (int, char **);
  fun = &next_main;
  //i = (*fun)(argc, argv);

  int (*fun2) (int, char *[]);
  fun2 = &next_main;
  //i = (*fun2)(argc, argv);

  i = 1;
  int lst[6] = { -1, 1 - 1, i, 2, 3 };
  for (int i = 0; i < 4; i++)
    {
      eputs ("i: ");
      eputs (itoa (lst[i]));
      eputs ("\n");
      if (lst[i + 1] != i)
        return 30 + i;
    }
  eputs ("foo" "bar");

  return 0;
}

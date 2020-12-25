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

struct foo
{
  int length;
  char buf[16];
};

struct bar
{
  struct
  {
    int x;
    int y;
    int z;
  };
};

#if __i386__
#define ptr_size 4
#define foo_size 20
#define bar_size 12
#elif __x86_64__
#define ptr_size 8
#define foo_size 24
#define bar_size 12
#endif


int
main ()
{
  char **p;
  if (sizeof (*p) != ptr_size)
    return 1;
  if (sizeof (**p) != 1)
    return 2;
  oputs ("size: ");
  oputs (itoa (sizeof (struct foo)));
  oputs ("\n");
  if (sizeof (struct foo) != 20)
    return 3;
  struct foo f;
  if (sizeof f != 20)
    return 4;
  if (sizeof (struct bar) != 12)
    return 5;
  return 0;
}

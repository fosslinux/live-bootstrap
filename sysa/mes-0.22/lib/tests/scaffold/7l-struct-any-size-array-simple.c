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

#if __MESC__
#define  __attribute__(x)
#endif

struct foo13
{
  int a;
  int b;
  int c;
  char d;
} __attribute__ ((packed));

struct foo13 tab14[3];

int
main ()
{
  unsigned char *p;

  tab14[1].a = -1;
  tab14[1].b = -2;
  tab14[1].c = -3;
  tab14[1].d = -4;

  if (tab14[1].d != -4)
    return 1;

  return 0;
}

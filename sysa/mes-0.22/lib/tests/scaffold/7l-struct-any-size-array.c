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

;

struct foo16
{
  int a;
  int b;
  int c;
  int d;
};

struct foo13 tab14[3];
struct foo16 tab16[3];

int
main ()
{
  unsigned char *p;

  tab14[1].a = -1;
  tab14[1].b = -1;
  tab14[1].c = -1;
  tab14[1].d = -1;

  p = &tab14;
  for (int i = 0; i < sizeof (struct foo13) * 2; i++)
    {
      if (i < 10)
        eputs (" ");
      eputs (itoa (i));
      eputs (": ");
      eputs (itoa (p[i]));
      eputs ("\n");
    }

  for (int i = 0; i < sizeof (struct foo13); i++)
    if (p[i] != 0)
      return 1 + i;

  for (int i = sizeof (struct foo13); i < 2 * sizeof (struct foo13); i++)
    if (p[i] != 255)
      return 1 + i;

  tab16[1].a = -1;
  tab16[1].b = -1;
  tab16[1].c = -1;
  tab16[1].d = -1;

  p = &tab16;
  for (int i = 0; i < sizeof (struct foo16) * 2; i++)
    {
      if (i < 10)
        eputs (" ");
      eputs (itoa (i));
      eputs (": ");
      eputs (itoa (p[i]));
      eputs ("\n");
    }

  for (int i = 0; i < sizeof (struct foo16); i++)
    if (p[i] != 0)
      return 1 + i;

  for (int i = sizeof (struct foo16); i < 2 * sizeof (struct foo16); i++)
    if (p[i] != 255)
      return 1 + i;

  return 0;
}

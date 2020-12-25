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

char *list[2] = { "foo\n", "bar\n" };

struct foo
{
  int a;
  int b;
  int c;
  unsigned char *d;
#if __MESC__ && __x86_64__
  int __align;
#endif
};

int
main ()
{
  char *pc = 0;
  void *pv = 0;
  int *pi = 0;
  char **ppc = 0;
  void **ppv = 0;
  int **ppi = 0;
  int int_size = sizeof (int);
  int ptr_size = sizeof (void *);
  int foo_size = sizeof (struct foo);
  oputs ("int_size:");
  oputs (itoa (int_size));
  oputs ("\n");
  oputs ("ptr_size:");
  oputs (itoa (ptr_size));
  oputs ("\n");
  oputs ("foo_size:");
  oputs (itoa (foo_size));
  oputs ("\n");
  // FIXME: add *14, *18
#if __i386__
  int foo_size_14 = 224;
  int foo_size_18 = 288;
#elif __x86_64__
  int foo_size_14 = 336;
  int foo_size_18 = 432;
#endif

  if (++pc != 1)
    return 1;
  if (++pv != 1)
    return 2;
  if (++pi != int_size)
    return 3;
  if (++ppc != ptr_size)
    return 4;
  if (++ppv != ptr_size)
    return 5;
  if (++ppi != ptr_size)
    return 6;
  if (pc + 1 != 2)
    return 7;
  if (pv + 1 != 2)
    return 8;
  if (pi + 1 != int_size << 1)
    return 9;
  if (ppc + 1 != ptr_size << 1)
    return 10;
  if (ppv + 1 != ptr_size << 1)
    return 11;
  if (ppi + 1 != ptr_size << 1)
    return 12;

  char **p = list;
  ++*p;
  eputs (*p);
  if (strcmp (*p, "oo\n"))
    return 13;
  --*p;
  eputs (*p);
  if (strcmp (*p, "foo\n"))
    return 14;

  struct foo *pfoo = 0;
  eputs ("pfoo=");
  eputs (itoa (pfoo));
  eputs ("\n");
  pfoo++;
  eputs ("pfoo=");
  eputs (itoa (pfoo));
  eputs ("\n");
  if (pfoo != foo_size)
    return 15;

  pfoo--;
  eputs ("pfoo=");
  eputs (itoa (pfoo));
  eputs ("\n");
  if (pfoo)
    return 16;

  pfoo++;
  eputs ("pfoo=");
  eputs (itoa (pfoo));
  eputs ("\n");
  if (pfoo != foo_size)
    return 17;

  long one = 1;
  long two = 2;
  pfoo = pfoo - one;
  eputs ("pfoo=");
  eputs (itoa (pfoo));
  eputs ("\n");
  if (pfoo)
    return 18;

  pfoo = pfoo + one;
  eputs ("pfoo=");
  eputs (itoa (pfoo));
  eputs ("\n");
  if (pfoo != foo_size)
    return 19;

  pfoo -= one;
  eputs ("pfoo=");
  eputs (itoa (pfoo));
  eputs ("\n");
  if (pfoo)
    return 20;

  pfoo += one;
  eputs ("pfoo=");
  eputs (itoa (pfoo));
  eputs ("\n");
  if (pfoo != foo_size)
    return 21;

  eputs ("&one: ");
  eputs (itoa (&one));
  eputs ("\n");
  eputs ("&two: ");
  eputs (itoa (&two));
  eputs ("\n");

  if (&one - 1 != &two)
    return 22;

  struct foo *sym = foo_size + foo_size;
  int i = sym + 16;
  eputs ("i=");
  eputs (itoa (i));
  eputs ("\n");
  if (i != foo_size_18)
    return 23;

  int d = 16;
  i = sym + d;
  eputs ("i=");
  eputs (itoa (i));
  eputs ("\n");
  if (i != foo_size_18)
    return 24;

  i = sym - 16;
  eputs ("i=");
  eputs (itoa (i));
  eputs ("\n");
  if (i != -foo_size_14)
    return 25;

  i = sym - d;
  eputs ("i=");
  eputs (itoa (i));
  eputs ("\n");
  if (i != -foo_size_14)
    return 26;

  i = sym - (struct foo *) foo_size;
  eputs ("i=");
  eputs (itoa (i));
  eputs ("\n");
  if (i != 1)
    return 27;

  pfoo = sym + 1;
  pfoo -= sym;
  eputs ("pfoo=");
  eputs (itoa (pfoo));
  eputs ("\n");
  if (pfoo != 1)
    return 28;

  return 0;
}

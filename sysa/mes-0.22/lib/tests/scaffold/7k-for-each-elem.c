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

struct section
{
  unsigned char *data;
  int offset;
};

struct sym
{
  char *name;
  int index;
};

struct sym tab3[3] = { "foo", 0, "bar", 1, "baz", 2 };
struct sym tab[] = { "foo", 0, "bar", 1, "baz", 2 };

struct section section;

#define for_each_elem(sec, startoff, elem, type) \
    for (elem = (type *) sec->data + startoff; \
         elem < (type *) (sec->data + sec->offset); elem++)
#define for_each_elem2(sec, startoff, elem, type) \
  elem = sec->data + sizeof (type) * startoff; \
  for (;elem < ((type *) (sec->data + sec->offset)); elem++)

int
main ()
{
#if __i386__
  int sym_size = 8;
#elif __GNUC__ && __x86_64__
  int sym_size = 16;
#elif  __MESC__ && __x86_64__
  int sym_size = 12;
#endif

  struct sym *p;
  p = tab3;
  section.data = tab;
  section.offset = 24;

  int size = sizeof (struct sym);
  eputs ("size=");
  eputs (itoa (size));
  eputs ("\n");
  if (size != sym_size)
    return 1;
  struct section *psection = &section;
  p = (struct sym *) psection->data + 1;
  struct sym *q = tab;
  int i = (int) p;
  i -= (int) q;
  eputs ("diff=");
  eputs (itoa (i));
  eputs ("\n");
  if (i != sym_size)
    return 2;

  for_each_elem (psection, 1, p, struct section)
  {
    eputs ("i=");
    eputs (itoa (p->index));
    eputs (" name=");
    eputs (p->name);
    eputs ("\n");
  }

  for_each_elem2 (psection, 1, p, struct section)
  {
    eputs ("i=");
    eputs (itoa (p->index));
    eputs (" name=");
    eputs (p->name);
    eputs ("\n");
  }

  return 0;
}

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

#include <stdlib.h>
#include <string.h>

void
qswap (void *a, void *b, size_t size)
{
  char *buf[8];
  memcpy (buf, a, size);
  memcpy (a, b, size);
  memcpy (b, buf, size);
}

size_t
qpart (void *base, size_t count, size_t size, int (*compare) (void const *, void const *))
{
  void *p = base + count * size;
  size_t i = 0;
  for (size_t j = 0; j < count; j++)
    {
      int c = compare (base + j * size, p);
      if (c < 0)
        {
#if 1                           //__x86_64__
          qswap (base + i * size, base + j * size, size);
#else
          int p1 = base + i * size;
          int p2 = base + j * size;
          qswap (p1, p2, size);
#endif
          i++;
        }
      else if (c == 0)
        i++;
    }
  if (compare (base + count * size, base + i * size) < 0)
    qswap (base + i * size, base + count * size, size);
  return i;
}

void
qsort (void *base, size_t count, size_t size, int (*compare) (void const *, void const *))
{
  if (count > 1)
    {
      int p = qpart (base, count - 1, size, compare);
      qsort (base, p, size, compare);
#if 1                           //__x86_64__
      qsort (base + p * size, count - p, size, compare);
#else
      int p1 = base + p * size;
      int p2 = count - p;
      qsort (p1, p2, size, compare);
#endif
    }
}

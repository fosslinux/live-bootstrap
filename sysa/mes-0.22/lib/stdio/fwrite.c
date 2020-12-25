/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
#include <unistd.h>

size_t
fwrite (void const *data, size_t size, size_t count, FILE * stream)
{
  if (__mes_debug () > 1)
    {
      eputs ("fwrite ");
      eputs (itoa ((int) (long) stream));
      eputs ("  ");
      eputs (itoa (size));
      eputs ("\n");
    }

  if (!size || !count)
    return 0;
  int filedes = (long) stream;
  int bytes = write (filedes, data, size * count);

  if (__mes_debug () > 2)
    {
      eputs (" => ");
      eputs (itoa (bytes));
      eputs ("\n");
    }

  if (bytes > 0)
    return bytes / size;
  return 0;
}

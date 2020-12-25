/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#if SYSTEM_LIBC
#error "SYSTEM_LIBC not supported"
#endif

#include <mes/lib.h>
#include <stdio.h>
#include <stdlib.h>

int
main (int argc, char *argv[])
{
  int size = 5000;
  oputs ("m!\n");
  //int *p = 0;
  char *p = 0;
  p = malloc (size);
  size = 5000;
  oputs ("p=");
  oputs (itoa (p));
  oputs ("\n");
  int i;
  for (i = 0; i < size; i = i + 1)
    {
      oputs ("set ");
      oputs (itoa (i));
      oputs ("\n");
      p[i] = i;
    }
  for (i = 0; i < size; i = i + 1)
    {
      oputs (itoa (i));
      oputs (": ");
      oputs (itoa (p[i]));
      oputs ("\n");
    }
  return 0;
}

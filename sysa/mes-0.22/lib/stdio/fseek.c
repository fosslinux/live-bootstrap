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

int
fseek (FILE * stream, long offset, int whence)
{
  int filedes = (long) stream;
  off_t pos = lseek (filedes, offset, whence);
  if (__mes_debug ())
    {
      eputs ("fread fd=");
      eputs (itoa (filedes));
      eputs ("  =>");
      eputs (itoa (pos));
      eputs ("\n");
    }
  if (pos >= 0)
    return 0;
  return -1;
}

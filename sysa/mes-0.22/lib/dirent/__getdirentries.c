/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright (C) 1993 Free Software Foundation, Inc.
 * Copyright © 2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

// Taken from GNU C Library 1.06.4

#include <mes/lib.h>
#include <dirent.h>
#include <stdio.h>
#include <unistd.h>

int
__getdirentries (int filedes, char *buffer, size_t nbytes, off_t * basep)
{
  if (basep)
    *basep = lseek (filedes, (off_t) 0, SEEK_CUR);

  return read (filedes, buffer, nbytes);
}

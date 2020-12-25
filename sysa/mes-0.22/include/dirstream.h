/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 *Copyright (C) 1993, 1995, 1996 Free Software Foundation, Inc.
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

#ifndef __MES_DIRSTREAM_H
#define __MES_DIRSTREAM_H 1

#if SYSTEM_LIBC && HAVE_DIRSTREAM_H
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#undef __MES_DIRSTREAM_H
#include_next <dirstream.h>

#else // ! SYSTEM_LIBC

#include <sys/types.h>

// Taken from GNU C Library 2.2.5

/* Directory stream type.  */
struct __dirstream
{
  int fd;                       /* File descriptor.  */

  char *data;                   /* Directory block.  */
  size_t allocation;            /* Space allocated for the block.  */
  size_t size;                  /* Total valid data in the block.  */
  size_t offset;                /* Current offset into the block.  */

  off_t filepos;                /* Position of next entry to read.  */
};

typedef struct __dirstream DIR;

#endif // ! SYSTEM_LIBC

#endif // __MES_DIRSTREAM_H

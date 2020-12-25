/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright (C) 1991, 1992 Free Software Foundation, Inc.
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

#ifndef __MES_DIRENT_H
#define __MES_DIRENT_H 1

#if SYSTEM_LIBC
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#undef __MES_DIRENT_H
#include_next <dirent.h>

#else // ! SYSTEM_LIBC

#include <dirstream.h>

// Taken from GNU C Library 1.06.4, 2.2.5

/*
 *	POSIX Standard: 5.1.2 Directory Operations	<dirent.h>
 */

#include <stddef.h>

int __getdirentries (int filedes, char *buffer, size_t nbytes, off_t * basep);

struct dirent
{
  ino_t d_ino;
  off_t d_off;
  unsigned short int d_reclen;
#if 0
  unsigned char d_type;
#endif
  char d_name[256];             /* We must not include limits.h! */
};

/* Open a directory stream on NAME.
   Return a DIR stream on the directory, or NULL if it could not be opened.  */
DIR *opendir (char const *name);

/* Close the directory stream DIRP.
   Return 0 if successful, -1 if not.  */
int closedir (DIR * dirp);

/* Read a directory entry from DIRP.
   Return a pointer to a `struct dirent' describing the entry,
   or NULL for EOF or error.  The storage returned may be overwritten
   by a later readdir call on the same DIR stream.  */
struct dirent *readdir (DIR * dirp);

/* Rewind DIRP to the beginning of the directory.  */
extern void rewinddir (DIR * dirp);

#endif // ! SYSTEM_LIBC

#endif // __MES_DIRENT_H

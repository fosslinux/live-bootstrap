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
#ifndef __MES_FCNTL_H
#define __MES_FCNTL_H 1

#if SYSTEM_LIBC
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#undef __MES_FCNTL_H
#include_next <fcntl.h>

#else // ! SYSTEM_LIBC

// *INDENT-OFF*
#if __linux__
#define O_RDONLY          0
#define O_WRONLY          1
#define O_RDWR            2
#define O_CREAT        0x40
#define O_EXCL         0x80
#define O_TRUNC       0x200
#define O_APPEND      0x400
#define O_DIRECTORY 0x10000
#elif __GNU__
#define	O_RDONLY	  1
#define	O_WRONLY	  2
#define	O_RDWR		  3
#define	O_CREAT	       0x10
#define	O_APPEND      0x100
#define	O_TRUNC	    0x10000
#else
#error platform not supported
#endif
// *INDENT-ON*

#define FD_CLOEXEC 1

#define F_DUPFD 0
#define F_GETFD 1
#define F_SETFD 2
#define F_GETFL 3
#define F_SETFL 4

#define creat(file_name, mode) open (file_name, O_WRONLY | O_CREAT | O_TRUNC, mode)
int dup (int old);
int dup2 (int old, int new);
int fcntl (int filedes, int command, ...);
int open (char const *s, int flags, ...);

#endif // ! SYSTEM_LIBC

#endif // __MES_FCNTL_H

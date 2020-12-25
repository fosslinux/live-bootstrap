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
#ifndef __MES_ERRNO_H
#define __MES_ERRNO_H 1

#if SYSTEM_LIBC
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#undef __MES_ERRNO_H
#include_next <errno.h>
#else // ! SYSTEM_LIBC

#ifndef __MES_ERRNO_T
#define __MES_ERRNO_T 1
typedef int error_t;
int errno;
#endif // !__MES_ERRNO_T

int errno;
#define ENOENT   2
#define EINTR    4
#define EIO      5
#define ENXIO    6
#define E2BIG    7
#define ENOEXEC  8
#define EBADF    9
#define	ECHILD  10
#define EAGAIN  11
#define ENOMEM  12
#define EACCES  13
#define EEXIST  17
#define EXDEV   18
#define ENOTDIR 20
#define EISDIR  21
#define EINVAL  22
#define EMFILE  24
#define ENOSPC  28
#define ESPIPE  29
#define EPIPE   32
#define ERANGE  34

#define ENAMETOOLONG 36
#define ENOSYS 38
#define ELOOP  40

#if !__MESC__
//extern char const *const sys_errlist[];
extern char *sys_errlist[];
extern int sys_nerr;
#endif // !__MESC__

#endif // ! SYSTEM_LIBC

#endif // __MES_ERRNO_H

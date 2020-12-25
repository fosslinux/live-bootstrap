/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#ifndef __MES_LIB_MINI_H
#define __MES_LIB_MINI_H

#if HAVE_CONFIG_H
#include <mes/config.h>
#endif

// CONSTANT STDIN 0
#ifndef STDIN
#define STDIN 0
#endif

// CONSTANT STDOUT 1
#ifndef STDOUT
#define STDOUT 1
#endif

// CONSTANT STDERR 2
#ifndef STDERR
#define STDERR 2
#endif

char **environ;
int __stdin;
int __stdout;
int __stderr;

int eputs (char const *s);
int puts (char const *s);
int oputs (char const *s);

#if SYSTEM_LIBC

#include <sys/types.h>
#include <unistd.h>

#else //!SYSTEM_LIBC

#ifndef _SIZE_T
#define _SIZE_T
#ifndef __SIZE_T
#define __SIZE_T
#ifndef __MES_SIZE_T
#define __MES_SIZE_T
#undef size_t
typedef unsigned long size_t;
#endif
#endif
#endif

#ifndef _SSIZE_T
#define _SSIZE_T
#ifndef __SSIZE_T
#define __SSIZE_T
#ifndef __MES_SSIZE_T
#define __MES_SSIZE_T
#undef ssize_t
#if __i386__
typedef int ssize_t;
#else
typedef long ssize_t;
#endif
#endif
#endif
#endif

#ifndef __MES_ERRNO_T
#define __MES_ERRNO_T 1
typedef int error_t;
int errno;
#endif // !__MES_ERRNO_T

size_t strlen (char const *s);
ssize_t _write ();
ssize_t write (int filedes, void const *buffer, size_t size);
#endif // !SYSTEM_LIBC

#endif //__MES_LIB_MINI_H

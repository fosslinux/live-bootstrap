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
#ifndef __MES_SYS_TYPES_H
#define __MES_SYS_TYPES_H 1

#if SYSTEM_LIBC
#undef __MES_SYS_TYPES_H
#include_next <sys/types.h>
#else // ! SYSTEM_LIBC

#include <endian.h>

#ifndef __MESCCLIB__
#define __MESCCLIB__ 15
#endif

#ifndef EOF
#define EOF -1
#endif

#ifndef NULL
#define NULL 0
#endif

#ifndef __MES_CLOCK_T
#define __MES_CLOCK_T
#undef clock_t
typedef long clock_t;
#endif

#ifndef __MES_DEV_T
#define __MES_DEV_T
#undef dev_t
typedef long dev_t;
#endif

#if !defined (__MES_FILE_T) && ! defined (_FILE_T)
#define __MES_FILE_T
#define _FILE_T
typedef long FILE;
#endif

#ifndef __MES_GID_T
#define __MES_GID_T
#undef gid_t
typedef unsigned gid_t;
#endif

#ifndef __MES_INO_T
#define __MES_INO_T
#undef ino_t
typedef unsigned long ino_t;
#endif

#ifndef __MES_INO64_T
#define __MES_INO64_T
#undef ino64_t
typedef unsigned long long ino64_t;
#endif

#if !defined (__MES_INTPTR_T) && !defined (__intptr_t_defined)
#define __MES_INTPTR_T
#define __intptr_t_defined
#undef intptr_t
typedef long intptr_t;
#undef uintptr_t
typedef unsigned long uintptr_t;
#endif

#ifndef __MES_OFF_T
#define __MES_OFF_T
#undef off_t
typedef long off_t;
#endif

#ifndef __MES_OFF64_T
#define __MES_OFF64_T
#undef off64_t
typedef unsigned long long off64_t;
#endif

#ifndef __MES_PID_T
#define __MES_PID_T
#undef pid_t
typedef int pid_t;
#endif

#ifndef __PTRDIFF_T
#define __PTRDIFF_T
#ifndef __MES_PTRDIFF_T
#define __MES_PTRDIFF_T
#undef ptrdiff_t
typedef long ptrdiff_t;
#endif
#endif

#ifndef __MES_SIGVAL_T
#define __MES_SIGVAL_T
#undef clock_t
typedef long sigval_t;
#endif

#ifndef __SIZE_T
#define __SIZE_T
#ifndef __MES_SIZE_T
#define __MES_SIZE_T
#undef size_t
typedef unsigned long size_t;
#endif
#endif

#ifndef __MES_SSIZE_T
#define __MES_SSIZE_T
#undef ssize_t
typedef long ssize_t;
#endif

#ifndef __MES_UID_T
#define __MES_UID_T
#undef uid_t
typedef unsigned uid_t;
#endif

#ifndef __WCHAR_T
#define __WCHAR_T
#ifndef __MES_WCHAR_T
#define __MES_WCHAR_T
#undef wchar_t
typedef int wchar_t;
#endif
#endif

#endif // ! SYSTEM_LIBC

#endif // __MES_SYS_TYPES_H

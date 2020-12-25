/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2018 Peter De Wachter <pdewacht@gmail.com>
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
#ifndef __MES_STDINT_H
#define __MES_STDINT_H 1

#if SYSTEM_LIBC
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#undef __MES_STDINT_H
#include_next <stdint.h>

#else // ! SYSTEM_LIBC

#undef unsigned
#undef uint8_t
#undef int8_t

#undef uint16_t
#undef int16_t

#undef uint32_t
#undef int32_t

#undef uint64_t
#undef int64_t

#undef uintptr_t
#undef intmax_t
#undef intptr_t
#undef uintmax_t
#undef ptrdiff_t

typedef unsigned char uint8_t;
typedef char int8_t;
typedef unsigned short uint16_t;
typedef short int16_t;
typedef unsigned uint32_t;
typedef int int32_t;
typedef unsigned long long uint64_t;
typedef long long int64_t;

typedef int intmax_t;
typedef unsigned uintmax_t;

#include <sys/types.h>

#define CHAR_BIT 8
#define CHAR_MAX 255
#define UCHAR_MAX 255

#define INT8_MAX 127
#define INT8_MIN (-INT8_MAX-1)
#define UINT8_MAX 255

#define INT16_MAX 32767
#define INT16_MIN (-INT16_MAX-1)
#define UINT16_MAX 65535

#define INT32_MAX 2147483647
#define INT32_MIN (-INT32_MAX-1)
#define UINT32_MAX 4294967295U

#define INT64_MAX 9223372036854775807LL
#define INT64_MIN (-INT64_MAX-1)
#define UINT64_MAX 18446744073709551615ULL

#define INT_MIN -2147483648
#define INT_MAX 2147483647
#if __i386__
#define LONG_MIN INT_MIN
#define LONG_MAX INT_MAX
#define UINT_MAX UINT32_MAX
#define ULONG_MAX UINT32_MAX
#define LLONG_MIN INT64_MIN
#define LLONG_MAX INT64_MAX
#define SIZE_MAX UINT32_MAX
#elif __x86_64__
#define LONG_MIN INT64_MIN
#define LONG_MAX INT64_MAX
#define UINT_MAX UINT32_MAX
#define ULONG_MAX UINT64_MAX
#define LLONG_MIN INT64_MIN
#define LLONG_MAX INT64_MAX
#define SIZE_MAX UINT64_MAX
#endif

#endif // ! SYSTEM_LIBC

#endif // __MES_STDINT_H

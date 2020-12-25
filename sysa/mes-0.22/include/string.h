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
#ifndef __MES_STRING_H
#define __MES_STRING_H 1

#if SYSTEM_LIBC
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#undef __MES_STRING_H
#include_next <string.h>

#else // ! SYSTEM_LIBC

#include <sys/types.h>

void *memchr (void const *block, int c, size_t size);
void *memcpy (void *dest, void const *src, size_t n);
void *memmove (void *dest, void const *src, size_t n);
void *memset (void *s, int c, size_t n);
void *memchr (void const *block, int c, size_t size);
int memcmp (void const *s1, void const *s2, size_t n);
void *memmem (void const *haystack, int haystack_len, void const *needle, int needle_len);
char *strcat (char *dest, char const *src);
char *strchr (char const *s, int c);
int strcasecmp (char const *s1, char const *s2);
int strcmp (char const *, char const *);
char *strcpy (char *dest, char const *src);
size_t strlen (char const *);
char *strncpy (char *to, char const *from, size_t size);
int strncmp (char const *, char const *, size_t);
char *strrchr (char const *s, int c);
char *strstr (char const *haystack, char const *needle);
char *strlwr (char *string);
char *strupr (char *string);


char *strerror (int errnum);
void perror (char const *message);

#endif // ! SYSTEM_LIBC

#endif // __MES_STRING_H

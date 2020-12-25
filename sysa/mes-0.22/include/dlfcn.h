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
#ifndef __MES_DLFCN_H
#define __MES_DLFCN_H 1

#if SYSTEM_LIBC
#undef __MES_DLFCN_H
#include_next <dlfcn.h>

#else // ! SYSTEM_LIBC

#define RTLD_LAZY	0x00001
#define RTLD_NOW	0x00002
#define	RTLD_BINDING_MASK   0x3
#define RTLD_NOLOAD	0x00004
#define RTLD_DEEPBIND	0x00008
#define RTLD_GLOBAL	0x00100
#define RTLD_LOCAL	0
#define RTLD_NODELETE	0x01000
#define RTLD_DEFAULT    0

void *dlopen (char const *filename, int flags);
int dlclose (void *handle);

#endif // ! SYSTEM_LIBC

#endif // __MES_DLFCN_H

/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright (C) 1996 Free Software Foundation, Inc.
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
#ifndef __MES_AR_H
#define __MES_AR_H 1

#if SYSTEM_LIBC
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#undef __MES_AR_H
#include_next <ar.h>

#else // ! SYSTEM_LIBC

// Taken from GNU C Library 2.2.5

/* Archive files start with the ARMAG identifying string.  Then follows a
   `struct ar_hdr', and as many bytes of member file data as its `ar_size'
   member indicates, for each member file.  */

#define ARMAG	"!<arch>\n"     /* String that begins an archive file.  */
#define SARMAG	8               /* Size of that string.  */

#define ARFMAG	"`\n"           /* String in ar_fmag at end of each header.  */

struct ar_hdr
{
  char ar_name[16];             /* Member file name, sometimes / terminated. */
  char ar_date[12];             /* File date, decimal seconds since Epoch.  */
  char ar_uid[6], ar_gid[6];    /* User and group IDs, in ASCII decimal.  */
  char ar_mode[8];              /* File mode, in ASCII octal.  */
  char ar_size[10];             /* File size, in ASCII decimal.  */
  char ar_fmag[2];              /* Always contains ARFMAG.  */
};

#endif // ! SYSTEM_LIBC

#endif // __MES_ARGZ_H

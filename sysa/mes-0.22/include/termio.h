/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
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
#ifndef __MES_TERMIO_H
#define __MES_TERMIO_H 1

#if SYSTEM_LIBC
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#undef __MES_TERMIO_H
#include_next <termio.h>

#else // ! SYSTEM_LIBC

#define TIOCGWINSZ	0x5413
#define TCGETA		0x5405
#define TCSETAW		0x5407

#define VTIME 5
#define VMIN 6

#define ISIG	0000001
#define ICANON	0000002
#define ECHO	0000010
#define ECHOK	0000040
#define ECHONL	0000100

#define ISTRIP	0000040
#define INLCR	0000100
#define ICRNL	0000400

#define   CS8	0000060
#define PARENB	0000400

struct winsize
{
  unsigned short ws_row;
  unsigned short ws_col;
  unsigned short ws_xpixel;
  unsigned short ws_ypixel;
};

struct termio
{
  unsigned short c_iflag;
  unsigned short c_oflag;
  unsigned short c_cflag;
  unsigned short c_lflag;
  unsigned char c_line;
  unsigned char c_cc[8];
};

#endif // ! SYSTEM_LIBC

#endif // __MES_TERMIO_H

/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <mes/lib.h>
#include <sys/ioctl.h>
#include <stdlib.h>
#include <string.h>
#include <termio.h>

#if !SYSTEM_LIBC
typedef unsigned char cc_t;
typedef unsigned int speed_t;
typedef unsigned int tcflag_t;

// Use termio, termios2?
#define NCCS 19
struct termios
{
  tcflag_t c_iflag;
  tcflag_t c_oflag;
  tcflag_t c_cflag;
  tcflag_t c_lflag;
  cc_t c_line;
  cc_t c_cc[NCCS];
};
#endif

struct ktermios
{
  tcflag_t c_iflag;
  tcflag_t c_oflag;
  tcflag_t c_cflag;
  tcflag_t c_lflag;
  cc_t c_line;
  cc_t c_cc[NCCS];
  speed_t c_ispeed;
  speed_t c_ospeed;
};

int
__tcgetattr (int filedes, struct termios *termios_p)
{
  struct ktermios kernel_termios;
  int r = ioctl3 (filedes, TCGETS, (long) &kernel_termios);

  termios_p->c_iflag = kernel_termios.c_iflag;
  termios_p->c_oflag = kernel_termios.c_oflag;
  termios_p->c_cflag = kernel_termios.c_cflag;
  termios_p->c_lflag = kernel_termios.c_lflag;
  termios_p->c_line = kernel_termios.c_line;
#if 0
  termios_p->c_ispeed = kernel_termios.c_ispeed;
  termios_p->c_ospeed = kernel_termios.c_ospeed;
#endif
  memcpy (&termios_p->c_cc[0], &kernel_termios.c_cc[0], NCCS * sizeof (cc_t));
  return r;
}

int
isatty (int filedes)
{
  struct termios term;
  return __tcgetattr (filedes, &term) == 0;
}

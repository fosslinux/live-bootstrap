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
#ifndef __MES_SYS_TIME_H
#define __MES_SYS_TIME_H 1

#if SYSTEM_LIBC
#undef __MES_SYS_TIME_H
#include_next <sys/time.h>

#else // ! SYSTEM_LIBC

struct timeval
{
  long tv_sec;
  long tv_usec;
};

struct timezone
{
  int tz_minuteswest;
  int tz_dsttime;
};

struct itimerval
{
  struct timeval it_interval;
  struct timeval it_value;
};

#define	ITIMER_REAL    0
#define	ITIMER_VIRTUAL 1
#define	ITIMER_PROF    2

int gettimeofday (struct timeval *tv, struct timezone *tz);
int setitimer (int which, struct itimerval const *new, struct itimerval *old);

#endif // ! SYSTEM_LIBC

#endif // __MES_SYS_TIME_H

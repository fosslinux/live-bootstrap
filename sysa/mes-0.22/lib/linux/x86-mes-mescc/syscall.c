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

#include <errno.h>
#include <linux/x86/syscall.h>

int
__sys_call (int sys_call)
{
  asm ("mov____0x8(%ebp),%eax !8");
  asm ("int____$0x80");
}

int
__sys_call1 (int sys_call, int one)
{
  asm ("mov____0x8(%ebp),%eax !8");
  asm ("mov____0x8(%ebp),%ebx !12");
  asm ("int____$0x80");
}

int
__sys_call2 (int sys_call, int one, int two)
{
  asm ("mov____0x8(%ebp),%eax !8");
  asm ("mov____0x8(%ebp),%ebx !12");
  asm ("mov____0x8(%ebp),%ecx !16");
  asm ("int____$0x80");
}

int
__sys_call3 (int sys_call, int one, int two, int three)
{
  asm ("mov____0x8(%ebp),%eax !8");
  asm ("mov____0x8(%ebp),%ebx !12");
  asm ("mov____0x8(%ebp),%ecx !16");
  asm ("mov____0x8(%ebp),%edx !20");
  asm ("int____$0x80");
}

int
__sys_call4 (int sys_call, int one, int two, int three, int four)
{
  asm ("mov____0x8(%ebp),%eax !8");
  asm ("mov____0x8(%ebp),%ebx !12");
  asm ("mov____0x8(%ebp),%ecx !16");
  asm ("mov____0x8(%ebp),%edx !20");
  asm ("mov____0x8(%ebp),%esi !24");
  asm ("int____$0x80");
}

int
_sys_call (int sys_call)
{
  int r = __sys_call (sys_call);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

int
_sys_call1 (int sys_call, int one)
{
  int r = __sys_call1 (sys_call, one);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

int
_sys_call2 (int sys_call, int one, int two)
{
  int r = __sys_call2 (sys_call, one, two);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

int
_sys_call3 (int sys_call, int one, int two, int three)
{
  int r = __sys_call3 (sys_call, one, two, three);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

int
_sys_call4 (int sys_call, int one, int two, int three, int four)
{
  int r = __sys_call4 (sys_call, one, two, three, four);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

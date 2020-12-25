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
#include <linux/x86_64/syscall.h>

// HMM, merge this with x86-mes-gcc/mes.c, doing something like
// #define R0 eax
// #define R1 ebx
//
// #define R0 rax
// #define R1 rdi

// *INDENT-OFF*
long
_sys_call (long sys_call)
{
  long r;
  asm (
       "mov     %1,%%rax\n\t"
       "syscall \n\t"
       "mov     %%rax,%0\n\t"
       : "=r" (r)
       : "rm" (sys_call)
       : "rax"
       );
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

long
_sys_call1 (long sys_call, long one)
{
  long r;
  asm (
       "mov     %1,%%rax\n\t"
       "mov     %2,%%rdi\n\t"
       "syscall \n\t"
       "mov     %%rax,%0\n\t"
       : "=r" (r)
       : "rm" (sys_call), "rm" (one)
       : "rax", "rdi"
       );
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

long
_sys_call2 (long sys_call, long one, long two)
{
  long r;
  asm (
       "mov     %1,%%rax\n\t"
       "mov     %2,%%rdi\n\t"
       "mov     %3,%%rsi\n\t"
       "syscall \n\t"
       "mov     %%rax,%0\n\t"
       : "=r" (r)
       : "rm" (sys_call), "rm" (one), "rm" (two)
       : "rax", "rdi", "rsi"
       );
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

long
_sys_call3 (long sys_call, long one, long two, long three)
{
  long r;
  asm (
       "mov     %2,%%rdi\n\t"
       "mov     %3,%%rsi\n\t"
       "mov     %4,%%rdx\n\t"
       "mov     %1,%%rax\n\t"
       "syscall \n\t"
       "mov     %%rax,%0\n\t"
       : "=r" (r)
       : "rm" (sys_call), "rm" (one), "rm" (two), "rm" (three)
       : "rax", "rdi", "rsi", "rdx"
       );
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

long
_sys_call4 (long sys_call, long one, long two, long three, long four)
{
  long r;
  asm (
       "mov     %2,%%rdi\n\t"
       "mov     %3,%%rsi\n\t"
       "mov     %4,%%rdx\n\t"
       "mov     %5,%%r10\n\t"
       "mov     %1,%%rax\n\t"
  //      );
  // asm (
       "syscall \n\t"
       "mov     %%rax,%0\n\t"
       : "=r" (r)
       : "rm" (sys_call), "rm" (one), "rm" (two), "rm" (three), "rm" (four)
       : "rax", "rdi", "rsi", "rdx", "r10"
       );
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

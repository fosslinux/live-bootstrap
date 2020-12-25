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

#include "mes/lib-mini.h"

#define SYS_exit   "0x3c"
#define SYS_write  "0x01"

// *INDENT-OFF*
void
_exit (int code)
{
  asm (
       "mov     $"SYS_exit",%%rax\n\t"
       "mov     %0,%%rdi\n\t"
       "syscall \n\t"
       : // no outputs "=" (r)
       : "rm" (code)
       );
  // not reached
  _exit (0);
}

ssize_t
_write (int filedes, void const *buffer, size_t size)
{
  long r;
  asm (
       "mov     $"SYS_write",%%rax\n\t"
       "mov     %1,%%rdi\n\t"
       "mov     %2,%%rsi\n\t"
       "mov     %3,%%rdx\n\t"
       "syscall \n\t"
       "mov     %%rax,%0\n\t"
       : "=r" (r)
       : "rm" (filedes), "rm" (buffer), "rm" (size)
       : "rax", "rdi", "rsi", "rdx"
       );
  return r;
}

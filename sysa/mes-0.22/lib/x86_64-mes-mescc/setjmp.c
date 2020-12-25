/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <setjmp.h>
#include <stdlib.h>

void
longjmp (jmp_buf env, int val)
{
#if 0                           //MES_CCAMD64
  asm ("push___%rdi");
#endif
  val = val == 0 ? 1 : val;
#if 0                           //MES_CCAMD64
  asm ("pop____%rdi");
  asm ("mov____0x8(%rdi),%rbp !0x00");  // env->__bp
  asm ("mov____0x8(%rdi),%rbx !0x08");  // env->__pc
  asm ("mov____0x8(%rdi),%rsp !0x10");  // env->__sp
  asm ("jmp____*%rbx");         // jmp *PC
#else
  asm ("mov____0x8(%rbp),%rbp !0x10");  // env*

  asm ("mov____0x8(%rbp),%rbx !0x08");  // env.__pc
  asm ("mov____0x8(%rbp),%rsp !0x10");  // env.__sp
  asm ("mov____0x8(%rbp),%rbp !0x00");  // env.__bp
  asm ("jmp____*%rbx");
#endif
  // not reached
  exit (42);
}

int
setjmp (__jmp_buf * env)
{
#if 0                           //MES_CCAMD64
  asm ("mov____%rbp,%rax");
  asm ("add____$i32,%rax %0x80");

  asm ("mov____0x8(%rax),%rsi !0x00");
  asm ("mov____%rsi,0x8(%rdi) !0x00");

  asm ("mov____0x8(%rax),%rsi !0x08");
  asm ("mov____%rsi,0x8(%rdi) !0x08");

  asm ("mov____%rax,%rsi");
  asm ("add____$i32,%rsi %0x10");
  asm ("mov____%rsi,0x8(%rdi) !0x10");
#else
  long *p = (long *) &env;
  env[0].__bp = p[-2];
  env[0].__pc = p[-1];
  env[0].__sp = (long) &env;
#endif
  return 0;
}

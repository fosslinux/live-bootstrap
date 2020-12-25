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

#include "mes/lib-mini.h"

void
_exit (int status)
{
#if 1                           // !MES_CCAMD64
  asm ("mov____0x8(%rbp),%rdi !0x10");
#endif

  asm ("mov____$i32,%rax SYS_exit");
  asm ("syscall");
}

void
_write (int filedes, void const *buffer, size_t size)
{
#if 1                           // !MES_CCAMD64
  asm ("mov____0x8(%rbp),%rdi !0x10");
  asm ("mov____0x8(%rbp),%rsi !0x18");
  asm ("mov____0x8(%rbp),%rdx !0x20");
#endif

  asm ("mov____$i32,%rax SYS_write");
  asm ("syscall");
}

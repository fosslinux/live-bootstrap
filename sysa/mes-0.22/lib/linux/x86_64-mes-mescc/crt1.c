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
int main (int argc, char *argv[], char *envp[]);

int
_start ()
{
  asm ("mov____$i8,%rax !0");
  asm ("movl___%eax,0x32 &__stdin");

  asm ("mov____$i8,%rax !1");
  asm ("movl___%eax,0x32 &__stdout");

  asm ("mov____$i8,%rax !2");
  asm ("movl___%eax,0x32 &__stderr");

#if 0                           //MES_CCAMD64
  asm ("add____$i32,%rbp %0x80");       // FIXME: corresponds to x86_64/as.scm function-preamble-fu
#endif
  asm ("mov____%rbp,%rax");
  asm ("add____$i8,%rax !8");

  asm ("mov____(%rax),%rax");
  asm ("add____$i8,%rax !0x03");

  asm ("shl____$i8,%rax !0x03");
  asm ("add____%rbp,%rax");

  // FIXME: 64-bit addresses...
  asm ("mov____%rax,0x32 &environ");
#if 0                           //MES_CCAMD64
  asm ("mov____%rax,%rdx");     // amd
#else
  asm ("push___%rax");          // bootstrap
#endif

  asm ("mov____%rbp,%rax");
  asm ("add____$i8,%rax !16");
#if 0                           //MES_CCAMD64
  asm ("mov____%rax,%rsi");     // amd
#else
  asm ("push___%rax");          // bootstrap
#endif

  asm ("mov____%rbp,%rax");
  asm ("add____$i8,%rax !8");
  asm ("mov____(%rax),%rax");
#if 0                           //MES_CCAMD64
  asm ("mov____%rax,%rdi");     // amd
#else
  asm ("push___%rax");          // bootstrap
#endif

  main ();

  asm ("mov____%rax,%rdi");
  asm ("mov____$i32,%rax %0x3c");
  asm ("syscall");
  asm ("hlt");
}

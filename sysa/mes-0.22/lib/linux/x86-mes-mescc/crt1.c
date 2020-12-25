/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
  asm ("mov____$i8,%eax !0");
  asm ("mov____%eax,0x32 &__stdin");

  asm ("mov____$i8,%eax !1");
  asm ("mov____%eax,0x32 &__stdout");

  asm ("mov____$i8,%eax !2");
  asm ("mov____%eax,0x32 &__stderr");

  asm ("mov____%ebp,%eax");
  asm ("add____$i8,%eax !4");

  asm ("movzbl_(%eax),%eax");
  asm ("add____$i8,%eax !3");

  asm ("shl____$i8,%eax !0x02");
  asm ("add____%ebp,%eax");
  asm ("mov____%eax,0x32 &environ");
  asm ("push___%eax");

  asm ("mov____%ebp,%eax");
  asm ("add____$i8,%eax !8");
  asm ("push___%eax");

  asm ("mov____%ebp,%eax");
  asm ("add____$i8,%eax !4");
  asm ("movzbl_(%eax),%eax");
  asm ("push___%eax");

  main ();

  asm ("mov____%eax,%ebx");
  asm ("mov____$i32,%eax %1");
  asm ("int____$0x80");
  asm ("hlt");
}

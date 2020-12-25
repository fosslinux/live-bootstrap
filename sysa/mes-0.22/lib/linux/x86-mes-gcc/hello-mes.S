/*
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

/** Commentary: */

/* Using gdb, esp. GUD in GNU Emacs is recommended
M-x gdb-display-disassembly-buffer
M-x gdb-display-io-buffer
set disassemble-next-line on
break _start
run
info registers
si
p/x $eax
RET
...
*/

/** Code: */

SYS_exit 	= 1		/* Linux syscalls.  */
SYS_write 	= 4
stdout		= 1		/* File discriptor */
	.globl _start		/* Magic linker symbol: --entry-address.  */

	.text			/* Program text. */
_start:
	mov	$SYS_write,%eax	/* System call function: write, in %eax.  */
	mov	$stdout,%ebx	/* 1st parameter: file descriptor, in %ebx. */
	mov	$hello,%ecx	/* 2nd parameter: address, in %ecx. */
	mov	$(bye-hello),%edx	/* 3rd parameter: byte count %edx. */
	int	$0x80		/* Call system.  */

	mov	$SYS_exit,%eax	/* System call function: exit, in %eax.  */
	mov	$0,%ebx		/* 1st parameter: exit status, in %ebx. */
	int	$0x80		/* Call system.  */
	hlt 			/* Should not be reached.  */

	.data
hello:	.ascii	"Hello, GNU Mes!\n"
bye:	.byte	0

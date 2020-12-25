### GNU Mes --- Maxwell Equations of Software
### Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
###
### This file is part of GNU Mes.
###
### GNU Mes is free software; you can redistribute it and/or modify it
### under the terms of the GNU General Public License as published by
### the Free Software Foundation; either version 3 of the License, or (at
### your option) any later version.
###
### GNU Mes is distributed in the hope that it will be useful, but
### WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.
###
### You should have received a copy of the GNU General Public License
### along with GNU Mes.  If not, see <http://www.gnu.org/licenses/>.

DEFINE sys_exit  01
DEFINE sys_write 04

:main
	mov____$i8,%eax !end_hello_message>hello_message
	mov____%eax,%ecx
	mov____$i8,%eax !0
	mov____%eax,%ebx
	mov____$i8,%eax sys_write
	int____$0x80

	mov____$i8,%eax !0
	mov____%eax,%ebx
	mov____$i8,%eax sys_exit
	int____$0x80

:ELF_data
:hello_message
"Hello, GNU Mes!
"
:end_hello_message
:HEX2_data

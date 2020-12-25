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

#ifndef __MES_SYSCALL_H
#define __MES_SYSCALL_H

long _sys_call (long sys_call);
long _sys_call1 (long sys_call, long one);
long _sys_call2 (long sys_call, long one, long two);
long _sys_call3 (long sys_call, long one, long two, long three);
long _sys_call4 (long sys_call, long one, long two, long three, long four);
long _sys_call6 (long sys_call, long one, long two, long three, long four, long five, long six);

#endif //__MES_SYSCALL_H

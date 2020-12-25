/* -*-comment-start: "//";comment-end:""-*-
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

/** Commentary:
    Inspired by implementation in GNU C Library:
    _hurd_exit
    Copyright (C) 1993-2016 Free Software Foundation, Inc.
 */

#include <gnu/hurd.h>
#include <gnu/hurd-types.h>
#include <gnu/syscall.h>
#include <mach/mach-init.h>

void
_exit (int status)
{
  __proc_mark_exit (_hurd_startup_data.portarray[INIT_PORT_PROC], status, 0);
  __task_terminate (mach_task_self ());
#if 0 // FIXME: this was needed?
  while (1) {* (int *) 0 = 0;}
#else
  asm ("hlt");
#endif
}

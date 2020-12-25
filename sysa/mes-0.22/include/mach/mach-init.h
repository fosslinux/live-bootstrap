/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 1993-2016 Free Software Foundation, Inc.
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
    Taken from GNU C Library
    Declarations and macros for the basic Mach things set at startup.
 */

#ifndef	_MACH_INIT_H

#define	_MACH_INIT_H	1

#include <mach/mach_types.h>

/* Return the current task's task port.  */
extern mach_port_t mach_task_self (void);
extern mach_port_t mach_host_self (void);

/* Kernel page size.  */
extern vm_size_t __vm_page_size;
extern vm_size_t vm_page_size;

/* Round the address X up to a page boundary.  */
#define round_page(x)	\
  ((((vm_offset_t) (x) + __vm_page_size - 1) / __vm_page_size) * \
   __vm_page_size)

/* Truncate the address X down to a page boundary.  */
#define trunc_page(x)	\
  ((((vm_offset_t) (x)) / __vm_page_size) * __vm_page_size)

#endif	/* mach_init.h */

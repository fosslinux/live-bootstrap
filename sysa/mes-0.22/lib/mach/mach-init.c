/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 1992-2016 Free Software Foundation, Inc.
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
 */

#define _GNU_SOURCE 1
#define __USE_GNU 1
#define __FILE_defined 1

#include <stdio.h>
#include <mach/mach-init.h>
#include <mach.h>
#include <unistd.h>

mach_port_t __mach_task_self_;
mach_port_t __mach_host_self_;
vm_size_t __vm_page_size = 0;

void
__mach_init (void)
{
  kern_return_t err;

#ifdef HAVE_HOST_PAGE_SIZE
  if (err = __host_page_size (mach_host_self (), &__vm_page_size))
    _exit (err);
#else
  {
    vm_statistics_data_t stats;
    if (err = __vm_statistics (mach_task_self (), &stats))
      _exit (err);
    __vm_page_size = stats.pagesize;
  }
#endif
}

extern mach_port_t __mach_host_self (void);

mach_port_t
mach_host_self (void)
{
  if (!__mach_host_self_)
    __mach_host_self_ = __mach_host_self ();
  return __mach_host_self_;
}

extern mach_port_t __mach_task_self (void);

mach_port_t
mach_task_self (void)
{
  if (!__mach_task_self_)
    __mach_task_self_ = __mach_task_self ();
  return __mach_task_self_;
}

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

#include <gnu/syscall.h>

struct mach_msg_int32_vm_statistics_data
{
  mach_msg_header_t header;
  mach_msg_type_t type_one; int one;
  mach_msg_type_t type_two; vm_statistics_data_t two;
};

kern_return_t
__vm_statistics (mach_port_t task, vm_statistics_data_t *vm_stats)
{
  struct mach_msg_int32_vm_statistics_data message = {0};
  message.header.msgh_size = sizeof (struct mach_msg);
  message.type_one = mach_msg_type_int32;
  message.type_two = mach_msg_type_int32;
  kern_return_t result = __syscall_get (task, SYS__vm_statistics,
                                        &message.header, sizeof (message));
  *vm_stats = message.two;
  return result;
}

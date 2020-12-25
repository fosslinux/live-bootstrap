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

kern_return_t
__task_get_special_port (mach_port_t task, int which, mach_port_t *port)
{
  struct mach_msg_2 message = {0};
  message.header.msgh_size = sizeof (struct mach_msg_1);
  message.type_one = mach_msg_type_int32;
  message.one = which;
  message.type_two = mach_msg_type_int32;
  message.two = 0;
  kern_return_t result = __syscall_get (task, SYS__task_get_special_port,
                                        &message.header, sizeof (message));
  if (result == KERN_SUCCESS)
    *port = message.two;
  return result;
}

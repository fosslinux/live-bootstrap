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

struct mach_msg_int_address
{
  mach_msg_header_t header;
  mach_msg_type_t type_one; int one;
  mach_msg_type_t type_two; vm_address_t two;
};

struct mach_msg_address_int_int
{
  mach_msg_header_t header;
  mach_msg_type_t type_one; vm_address_t one;
  mach_msg_type_t type_two; vm_size_t two;
  mach_msg_type_t type_three; boolean_t three;
};

const mach_msg_type_t mach_msg_type_boolean =
  {
   (unsigned char) MACH_MSG_TYPE_BOOLEAN,    // msgt_name
   32,                                       // msgt_size
   1,                                        // msgt_number
   1,                                        // msgt_inline
   0,                                        // msgt_longform
   0,                                        // msgt_deallocate
   0                                         // msgt_unused
  };

kern_return_t
__vm_allocate (mach_port_t task, vm_address_t *address, vm_size_t size, boolean_t anywhere)
{
  union message
  {
    struct mach_msg_address_int_int request;
    struct mach_msg_int_address reply;
  };
  union message message = {0};
  message.request.header.msgh_size = sizeof (message.request);
  message.request.type_one = mach_msg_type_int32;
  message.request.one = *address;
  message.request.type_two = mach_msg_type_int32;
  message.request.two = size;
  message.request.type_three = mach_msg_type_boolean;
  message.request.three = anywhere;

  kern_return_t result = __syscall_get (task, SYS__vm_allocate,
                                        &message.request.header,
                                        sizeof (message.reply));

  if (message.reply.one != KERN_SUCCESS)
    return message.reply.one;

  if (result == KERN_SUCCESS)
    *address = message.reply.two;
  return result;
}

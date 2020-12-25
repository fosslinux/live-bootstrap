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

struct mach_msg_int_pointer
{
  mach_msg_header_t header;
  mach_msg_type_t type_one; int one;
  mach_msg_type_long_t type_two; union {char *two; char pointer[2048];};
};

kern_return_t
__io_read (io_t io, data_t *data, mach_msg_type_number_t *read, loff_t offset, vm_size_t size)
{
  union message
  {
    struct mach_msg_loff_int request;
    struct mach_msg_int_pointer reply;
  };
  union message message = {0};
  message.request.header.msgh_size = sizeof (message.request);
  message.request.type_one = mach_msg_type_int64;
  message.request.one = offset;
  message.request.type_two = mach_msg_type_int32;
  message.request.two = size;

  kern_return_t result = __syscall_put (io, SYS__io_read,
                                        &message.request.header,
                                        sizeof (message.reply));

  if (result == KERN_SUCCESS)
    {
      *read = message.reply.type_two.msgtl_number;
      *data = message.reply.pointer;
    }
  else
    {
      *read = 0;
      *data = 0;
    }
  return result;
}

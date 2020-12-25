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

struct mach_msg_pointer_loff
{
  mach_msg_header_t header;
  mach_msg_type_long_t type_one; char *one;
  mach_msg_type_t type_two; loff_t two;
};

kern_return_t
__io_write (io_t io, data_t data, mach_msg_type_number_t size, loff_t offset, vm_size_t *wrote)
{
  struct mach_msg_pointer_loff message = {0};
  message.header.msgh_size = sizeof (struct mach_msg_pointer_loff);
  message.type_one = mach_msg_type_pointer;
  message.one = data;
  message.type_two = mach_msg_type_int64;
  message.two = offset;

  message.type_one.msgtl_number = size;
  message.type_one.msgtl_header.msgt_inline = 0;
  message.header.msgh_bits = MACH_MSGH_BITS_COMPLEX;

  kern_return_t result = __syscall_put (io, SYS__io_write,
                                        &message.header,
                                        sizeof (struct mach_msg_2));
  if (result == KERN_SUCCESS)
    *wrote = message.two;
  return result;
}

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
#include <string.h>
#include <sys/stat.h>

struct mach_msg_string_int_int
{
  mach_msg_header_t header;
  mach_msg_type_t type_one; string_t one;
  mach_msg_type_t type_two; int two;
  mach_msg_type_t type_three; int three;
};

struct mach_msg_int_int_string_int
{
  mach_msg_header_t header;
  mach_msg_type_t type_one; int one;
  mach_msg_type_t type_two; int two;
  mach_msg_type_t type_three; string_t three;
  mach_msg_type_t type_four; int four;
};

mach_msg_type_t mach_msg_type_file_name =
  {
   /* msgt_name = */		(unsigned char) MACH_MSG_TYPE_STRING_C,
   /* msgt_size = */		8,
   /* msgt_number = */		1024,
   /* msgt_inline = */		1,
   /* msgt_longform = */        0,
   /* msgt_deallocate = */	0,
   /* msgt_unused = */		0
  };

kern_return_t __dir_lookup (file_t start_dir, string_t file_name, int flags, mode_t mode, retry_type *do_retry, string_t retry_name, mach_port_t *port)
{
  union message
  {
    struct mach_msg_string_int_int request;
    struct mach_msg_int_int_string_int reply;
  };
  union message message = {0};
  message.request.header.msgh_size = sizeof (message.request);
  message.request.type_one = mach_msg_type_file_name;
  strcpy (message.request.one, file_name);
  message.request.type_two = mach_msg_type_int32;
  message.request.two = flags;
  message.request.type_three = mach_msg_type_int32;
  message.request.three = mode;

  kern_return_t result = __syscall_get (start_dir, SYS__dir_lookup,
                                        &message.request.header,
                                        sizeof (message.reply));
  if (message.reply.one != KERN_SUCCESS)
    return message.reply.one;
  *port = message.reply.four;
  return result;
}

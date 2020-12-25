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

#include <mach/mach_traps.h>

mach_msg_type_t mach_msg_type_int32 =
  {
   .msgt_name = (unsigned char) MACH_MSG_TYPE_INTEGER_32, // msgt_name
   .msgt_size = 32,       // msgt_size
   .msgt_number = 1,      // msgt_number
   .msgt_inline = 1,      // msgt_inline
   .msgt_longform = 0,    // msgt_longform
   .msgt_deallocate = 0,  // msgt_deallocate
   .msgt_unused = 0       // msgt_unused
  };

mach_msg_type_long_t mach_msg_type_pointer =
  {
   {
    0, // msgt_name
    0, // msgt_size
    0, // msgt_number
    1, // msgt_inline    FIXME: we always outline...
    1, // msgt_longform
    0, // msgt_deallocate
    0, // msgt_unused
   },
   MACH_MSG_TYPE_CHAR, // msgtl_name
   8, // msgtl_size
   2048, // msgtl_number
  };

mach_msg_type_t mach_msg_type_int64 =
  {
   (unsigned char) MACH_MSG_TYPE_INTEGER_64, // msgt_name
   64, // msgt_size
   1,  // msgt_number
   1,  // msgt_inline
   0,  // msgt_longform
   0,  // msgt_deallocate
   0,  // msgt_unused
  };

kern_return_t
__syscall (mach_port_t port, int sys_call)
{
  struct mach_msg message =
    {
     {
      MACH_MSGH_BITS (MACH_MSG_TYPE_COPY_SEND, MACH_MSG_TYPE_MAKE_SEND_ONCE),
      0,
      port,
      {__mach_reply_port (),},
      0,
      sys_call,
     }
    };
  return __mach_msg (&message.header,
                     MACH_SEND_MSG|MACH_RCV_MSG|MACH_MSG_OPTION_NONE,
                     sizeof (message),
                     sizeof (message),
                     message.header.msgh_local_port,
                     MACH_MSG_TIMEOUT_NONE,
                     MACH_PORT_NULL);
}

kern_return_t
__syscall2 (mach_port_t port, int sys_call, int one, int two)
{
  struct mach_msg_2 message =
    {
     {
      MACH_MSGH_BITS (MACH_MSG_TYPE_COPY_SEND, MACH_MSG_TYPE_MAKE_SEND_ONCE),
      0,
      port,
      {__mach_reply_port (),},
      0,
      sys_call,
     },
     mach_msg_type_int32, one,
     mach_msg_type_int32, two,
    };
  return __mach_msg (&message.header,
                     MACH_SEND_MSG|MACH_RCV_MSG|MACH_MSG_OPTION_NONE,
                     sizeof (message),
                     sizeof (message),
                     message.header.msgh_local_port,
                     MACH_MSG_TIMEOUT_NONE,
                     MACH_PORT_NULL);
}

kern_return_t
__syscall_get (mach_port_t port, int sys_call, mach_msg_header_t *message, size_t size)
{
  message->msgh_bits = MACH_MSGH_BITS (MACH_MSG_TYPE_COPY_SEND, MACH_MSG_TYPE_MAKE_SEND_ONCE);
  message->msgh_remote_port = port;
  message->msgh_local_port = __mach_reply_port ();
  message->msgh_seqno = 0;
  message->msgh_id = sys_call;
  return __mach_msg (message,
                     MACH_SEND_MSG|MACH_RCV_MSG|MACH_MSG_OPTION_NONE,
                     message->msgh_size,
                     size,
                     message->msgh_local_port,
                     MACH_MSG_TIMEOUT_NONE,
                     MACH_PORT_NULL);
}

kern_return_t
__syscall_put (mach_port_t port, int sys_call, mach_msg_header_t *message, size_t size)
{
  message->msgh_bits |= MACH_MSGH_BITS (MACH_MSG_TYPE_COPY_SEND, MACH_MSG_TYPE_MAKE_SEND_ONCE);
  message->msgh_remote_port = port;
  message->msgh_local_port = __mach_reply_port ();
  message->msgh_seqno = 0;
  message->msgh_id = sys_call;
  return __mach_msg (message,
                     MACH_SEND_MSG|MACH_RCV_MSG|MACH_MSG_OPTION_NONE,
                     message->msgh_size,
                     size,
                     message->msgh_local_port,
                     MACH_MSG_TIMEOUT_NONE,
                     MACH_PORT_NULL);
}

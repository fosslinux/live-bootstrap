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

static void
mach_startup_info2hurd_startup_data (struct mach_msg_startup_info *info,
                                     struct hurd_startup_data *data)
{
  data->flags = info->flags;
  data->dtable = info->dtable;
  data->dtable_count = info->dtableType.msgtl_number;

  data->argp = info->argv;
  data->arg_size = info->argvType.msgtl_number;

  data->envp = info->envp;
  data->env_size = info->envpType.msgtl_number;

  data->portarray = info->portarray;
  data->portarray_count = info->portarrayType.msgtl_number;

  data->intarray = info->intarray;
  data->intarray_count = info->intarrayType.msgtl_number;

  data->stack_base = info->stack_base;
  data->stack_count = info->stack_size;

  data->phdr = info->phdr;
  data->phdr_count = info->phdr_size;

  data->user_entry = info->user_entry;
}

kern_return_t
__exec_startup_get_data (mach_port_t bootstrap, struct hurd_startup_data *data)
{
  union message
    {
      mach_msg_header_t header;
      struct mach_msg_startup_info info;
    };
  union message message;
  message.header.msgh_size = sizeof (struct mach_msg);
  kern_return_t result = __syscall_get (bootstrap, SYS__exec_startup_get_info,
                                        &message.header, sizeof (message));


  mach_startup_info2hurd_startup_data (&message.info, data);
  return result;
}

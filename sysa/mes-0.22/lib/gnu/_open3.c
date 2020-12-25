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

/** Commentary:
    Inspired by implementation in GNU C Library:
    __hurd_file_name_lookup, __hurd_file_name_lookup_retry
    Copyright (C) 1992-2016 Free Software Foundation, Inc.
 */

#include <gnu/hurd.h>
#include <gnu/hurd-types.h>
#include <gnu/syscall.h>
#include <mach/mach-init.h>

int
_open3 (char const *file_name, int flags, int mode)
{
  mach_port_t port;
  int do_retry;
  char retry_name[1024];
  int start_dir = (file_name[0] == '/') ? INIT_PORT_CRDIR : INIT_PORT_CWDIR;
  mach_port_t start_port = _hurd_startup_data.portarray[start_dir];
  while (file_name[0] == '/')
    file_name++;
  error_t e = __dir_lookup (start_port, file_name, flags, mode, &do_retry, retry_name, &port);
  if (e)
    return -1;
  int fd = _hurd_dtable_count++;
  _hurd_dtable[fd] = port;
  return fd;
}

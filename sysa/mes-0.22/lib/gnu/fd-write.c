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
    _hurd_fd_write -- write to a file descriptor; handles job control et al.
    Copyright (C) 1993-2016 Free Software Foundation, Inc.
 */

#include <errno.h>
#include <unistd.h>
#include <gnu/hurd.h>
#include <gnu/syscall.h>

error_t
fd_write (mach_port_t port, void const *buffer, size_t *size, loff_t offset)
{
  mach_msg_type_number_t wrote = 0;
  error_t err = __io_write (port, buffer, *size, 0, &wrote);
  if (! err)
    *size = wrote;

  return err;
}

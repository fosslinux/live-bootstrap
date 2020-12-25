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
    _hurd_fd_read
    Copyright (C) 1993-2016 Free Software Foundation, Inc.
 */

#include <gnu/hurd.h>
#include <gnu/syscall.h>
#include <string.h>
#include <errno.h>

error_t
fd_read (mach_port_t port, void *buffer, size_t *read, loff_t offset)
{
  char *data;
  error_t e = __io_read (port, &data, read, offset, *read);
  if (!e)
    memcpy (buffer, data, *read);
  return e;
}

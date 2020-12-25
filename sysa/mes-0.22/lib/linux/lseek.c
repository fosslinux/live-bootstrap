/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <mes/lib.h>
#include <linux/syscall.h>
#include <syscall.h>
#include <stdio.h>
#include <sys/types.h>

#if !__MESC__ /* FIXME: We want bin/mes-mescc's x86-linux sha256sum to stay the same. */
off_t
_lseek (int filedes, off_t offset, int whence)
{
  return _sys_call3 (SYS_lseek, (int) filedes, (long) offset, (int) whence);
}
#endif

off_t
lseek (int filedes, off_t offset, int whence)
{
#if !__MESC__ /* FIXME: We want bin/mes-mescc's x86-linux sha256sum to stay the same. */
  if (_lseek (filedes, 0, SEEK_CUR) == -1)
    return -1;
#endif
  size_t skip = __buffered_read_clear (filedes);
  if (whence == SEEK_CUR)
    offset -= skip;
  return _sys_call3 (SYS_lseek, (int) filedes, (long) offset, (int) whence);
}

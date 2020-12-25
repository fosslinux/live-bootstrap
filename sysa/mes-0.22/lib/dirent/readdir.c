/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright (C) 1991,92,93,94,95,96,97,99,2000 Free Software Foundation, Inc.
 * Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

// Taken from GNU C Library 2.2.5

#include <errno.h>
#include <limits.h>
#include <stddef.h>
#include <string.h>
#include <dirent.h>
#include <unistd.h>
#include <sys/types.h>
#include <assert.h>

#include <dirstream.h>

int getdents (int filedes, char *buffer, size_t nbytes);

/* Read a directory entry from DIRP.  */
struct dirent *
readdir (DIR * dirp)
{
  struct dirent *dp;
  int saved_errno = errno;

  do
    {
      size_t reclen;

      if (dirp->offset >= dirp->size)
        {
          /* We've emptied out our buffer.  Refill it.  */

          size_t maxread;
          ssize_t bytes;

          maxread = dirp->allocation;

#if 0
          off_t base;
          bytes = __getdirentries (dirp->fd, dirp->data, maxread, &base);
#else
          bytes = getdents (dirp->fd, dirp->data, maxread);
#endif
          if (bytes <= 0)
            {
              /* Don't modifiy errno when reaching EOF.  */
              if (bytes == 0)
                errno = saved_errno;
              dp = 0;
              break;
            }
          dirp->size = (size_t) bytes;

          /* Reset the offset into the buffer.  */
          dirp->offset = 0;
        }

      dp = (struct dirent *) &dirp->data[dirp->offset];

      reclen = dp->d_reclen;
      dirp->offset += reclen;
      dirp->filepos = dp->d_off;

      /* Skip deleted files.  */
    }
  while (dp->d_ino == 0);

  return dp;
}

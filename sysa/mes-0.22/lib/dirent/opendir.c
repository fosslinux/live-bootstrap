/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright (C) 1991-1996,98,2000,2001 Free Software Foundation, Inc.
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

#include <mes/lib.h>
#include <errno.h>
#include <limits.h>
#include <stddef.h>
#include <stdlib.h>
#include <dirent.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdio.h>

#include <dirstream.h>

/* Open a directory stream on NAME.  */
DIR *
opendir (char const *name)
{
  DIR *dirp;
  struct stat statbuf;
  int fd;
  size_t allocation;
  int save_errno;

  if (name[0] == '\0')
    {
      /* POSIX.1-1990 says an empty name gets ENOENT;
         but `open' might like it fine.  */
      errno = ENOENT;
      return 0;
    }

  fd = open (name, O_RDONLY | O_DIRECTORY);
  if (fd < 0)
    return 0;

  if (fstat (fd, &statbuf) < 0)
    goto lose;

  if (fcntl (fd, F_SETFD, FD_CLOEXEC) < 0)
    goto lose;

  allocation = statbuf.st_blksize;

  dirp = (DIR *) calloc (1, sizeof (DIR) + allocation);
  if (!dirp)
  lose:
    {
      save_errno = errno;
      close (fd);
      errno = save_errno;
      return 0;
    }
  dirp->data = (char *) (dirp + 1);
  dirp->allocation = allocation;
  dirp->fd = fd;

  return dirp;
}

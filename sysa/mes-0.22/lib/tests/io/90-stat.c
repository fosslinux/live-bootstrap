/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
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

#include <mes/lib.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <sys/stat.h>


#if __i386__
#define stat xstat

struct stat
{
  unsigned long st_dev;
  unsigned long st_ino;
  unsigned short st_mode;
  unsigned short st_nlink;
  unsigned short st_uid;
  unsigned short st_gid;
  unsigned long st_rdev;
  long st_size;
  unsigned int st_blksize;
  unsigned int st_blocks;
  long st_atime;
  unsigned long st_atime_usec;
  long st_mtime;
  unsigned long st_mtime_usec;
  long st_ctime;
  unsigned long st_ctime_usec;
  unsigned int __foo0;
  unsigned int __foo1;
};
#endif

int
main ()
{
  // char buf[20];
  // strcpy (buf, "Hello");
  // eputs ("buf="); eputs (buf); eputs ("\n");
  // strcat (buf, ", ");
  // eputs ("buf="); eputs (buf); eputs ("\n");
  // strncat (buf, "world!XXX", 6);
  // eputs ("buf="); eputs (buf); eputs ("\n");
  // if (strcmp (buf, "Hello, world!"))
  //   return 1;

  // char *name = "boo";
  // errno = 0;
  // fprintf (stderr, "%s: %s\n", name, strerror (errno));
  int fd = open ("../COPYING", 0);

  struct stat sbuf;

  if (fd < 0)
    return 2;

  int r = fstat (fd, &sbuf);
  if (r < 0)
    return 1;

  eputs ("st_dev=");
  eputs (itoa (sbuf.st_dev));
  eputs ("\n");
  eputs ("st_ino=");
  eputs (itoa (sbuf.st_ino));
  eputs ("\n");
  eputs ("st_mode=");
  eputs (itoa (sbuf.st_mode));
  eputs ("\n");
  eputs ("st_nlink=");
  eputs (itoa (sbuf.st_nlink));
  eputs ("\n");
  eputs ("st_uid=");
  eputs (itoa (sbuf.st_uid));
  eputs ("\n");
  eputs ("st_gid=");
  eputs (itoa (sbuf.st_gid));
  eputs ("\n");
  eputs ("st_rdev=");
  eputs (itoa (sbuf.st_rdev));
  eputs ("\n");
  eputs ("st_size=");
  eputs (itoa (sbuf.st_size));
  eputs ("\n");

  eputs ("st_blksize=");
  eputs (itoa (sbuf.st_blksize));
  eputs ("\n");
  eputs ("st_blocks=");
  eputs (itoa (sbuf.st_blocks));
  eputs ("\n");

  eputs ("st_atime=");
  eputs (itoa (sbuf.st_atime));
  eputs ("\n");
  //eputs ("st_atime_nsec="); eputs (itoa (sbuf.st_atime_nsec)); eputs ("\n");

  eputs ("st_mtime=");
  eputs (itoa (sbuf.st_mtime));
  eputs ("\n");
  //eputs ("st_mtime_nsec="); eputs (itoa (sbuf.st_mtime_nsec)); eputs ("\n");

  eputs ("st_ctime=");
  eputs (itoa (sbuf.st_ctime));
  eputs ("\n");
  //eputs ("st_ctime_nsec="); eputs (itoa (sbuf.st_ctime_nsec)); eputs ("\n");

  eputs ("size:");
  eputs (itoa (sizeof (struct stat)));
  eputs ("\n");
  return 0;
}

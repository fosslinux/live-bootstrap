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

#include <mes/lib.h>
#include <fcntl.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int
main (int argc, char const *argv[])
{
  eputs ("test:getenv\n");
  char file_name[PATH_MAX];
  char *srcdir = getenv ("abs_top_srcdir");
  if (! srcdir) // for running by hand
    srcdir = ".";
  eputs ("srcdir=");
  eputs (srcdir);
  eputs ("\n");
  strcpy (file_name, srcdir);
  strcpy (file_name + strlen (srcdir), "/lib/tests/posix/data/open-read");
  eputs ("test open:");
  eputs (file_name);
  eputs ("\n");
  int filedes = open (file_name, O_RDONLY, 0);
  if (filedes <= 2)
    return 1;
  char buf[20];
  int n = read (filedes, buf, sizeof (buf));
  if (n != 5)
    return 2;
  if (strcmp (buf, "hello"))
    return 3;
  eputs ("test read: ");
  eputs (buf);
  eputs ("\n");
  return 0;
}

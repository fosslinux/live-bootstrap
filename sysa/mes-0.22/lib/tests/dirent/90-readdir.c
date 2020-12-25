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

#include <dirent.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

int dot_seen = 0;
int dot_dot_seen = 0;
int dir_seen = 0;
int file_seen = 0;
int link_seen = 0;

void
check_seen (char const* name)
{
  if (!strcmp (name, "."))
    dot_seen = 1;
  if (!strcmp (name, ".."))
    dot_dot_seen = 1;
  if (!strcmp (name, "dir"))
    dir_seen = 1;
  if (!strcmp (name, "file"))
    file_seen = 1;
  if (!strcmp (name, "link"))
    link_seen = 1;
}

int
main ()
{
  DIR *d = opendir ("../lib/tests/dirent/readdir-fu");
  if (d)
    return 1;
  if (errno != ENOENT)
    return 2;

  d = opendir ("../lib/tests/dirent/90-readdir.c");
  if (d)
    return 3;
  if (errno != ENOTDIR)
    return 4;

  errno = 0;
  d = opendir ("../lib/tests/dirent/readdir.dir");
  if (!d)
    return 5;

  if (errno)
    return 6;

  int i = 0;

  struct dirent *entry = readdir (d);
  if (!entry)
    return 7;
  oputs (entry->d_name);
  oputs ("\n");
  check_seen (entry->d_name);

  entry = readdir (d);
  if (!entry)
    return 8;
  oputs (entry->d_name);
  oputs ("\n");
  check_seen (entry->d_name);

  entry = readdir (d);
  if (!entry)
    return 9;
  oputs (entry->d_name);
  oputs ("\n");
  check_seen (entry->d_name);

  entry = readdir (d);
  if (!entry)
    return 10;
  oputs (entry->d_name);
  oputs ("\n");
  check_seen (entry->d_name);

  entry = readdir (d);
  if (!entry)
    return 11;
  oputs (entry->d_name);
  oputs ("\n");
  check_seen (entry->d_name);

  entry = readdir (d);
  if (entry)
    return 12;

  if (!dot_seen)
    return 13;

  if (!dot_dot_seen)
    return 14;

  if (!dir_seen)
    return 15;

  if (!file_seen)
    return 16;

  if (!link_seen)
    return 17;

  return 0;
}

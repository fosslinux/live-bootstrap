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
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>

int
main ()
{
  int fd = open ("../COPYING", O_RDONLY);
  if (fd <= 0)
    return 1;
  FILE *f = fdopen (fd, "r");
  int r = fseek (f, 0, SEEK_CUR);
  if (r != 0)
    return 2;
  int pos = ftell (f);
  if (pos != 0)
    return 3;

  r = fseek (f, 0, SEEK_END);
  if (r != 0)
    return 4;

  pos = ftell (f);
  eputs ("size=");
  eputs (itoa (pos));
  eputs ("\n");
  if (pos != 35147)
    return 5;
  r = fseek (f, 0, SEEK_SET);

  char buf[4096];
  fgets (buf, 200, f);
  eputs ("buf:");
  eputs (buf);
  if (strcmp (buf, "                    GNU GENERAL PUBLIC LICENSE\n"))
    return 6;

  return 0;
}

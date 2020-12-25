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
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int
main ()
{
  char *temp = "COPYING.tmp";
  char *new = "COPYING.new";

  unlink (temp);
  unlink (new);

  FILE *t = fopen (temp, "wb+");
  FILE *n = fopen (new, "wb");

  char *header = "!<header>\n";
  fwrite (header, strlen (header), 1, n);

  char *data = "foo bar baz\n";
  fwrite (data, strlen (data), 1, t);

  fseek (t, 0, SEEK_END);
  int size = ftell (t);
  fprintf (stderr, "  size=>%d\n", size);
  fseek (t, 0, SEEK_SET);
  char *p = (char *) malloc (size + 1);
  fread (p, size, 1, t);
  fwrite (p, size, 1, n);

  char header_plus_data[200];
  strcpy (header_plus_data, header);
  strcat (header_plus_data, data);

  FILE *test = fopen (new, "r");
  char buf[200];
  fflush (n);
  fread (buf, strlen (header_plus_data), 1, test);
  eputs ("buf=");
  eputs (buf);
  eputs ("\n");
  if (strcmp (buf, header_plus_data))
    return 1;

  if (access (temp, R_OK))
    return 22;

  unlink (temp);

  if (!access (temp, R_OK))
    return 3;

  unlink (new);

  return 0;
}

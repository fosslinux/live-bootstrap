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
dump (char const *name, char const *contents)
{
  unlink (name);
  FILE *f = fopen (name, "w");
  fwrite (contents, strlen (contents), 1, f);
  fclose (f);
}

int
main ()
{
  char *line = "The first line.\n";
  char *contents = "The first line.\nThe second line.\nThe last line.\n";
  char *end = "That's all folks!\n";

  char *tmp = "foo";

  dump (tmp, contents);

  FILE *t = fopen (tmp, "r+");
  if (t <= 0)
    return 1;

  char buf[80];
  memset (buf, 0, sizeof (buf));
  fread (buf, strlen (line), 1, t);
  eputs ("buf=");
  eputs (buf);
  eputs ("\n");
  if (strcmp (buf, line))
    return 2;

  fwrite (end, strlen (end), 1, t);

  fseek (t, 0, SEEK_SET);
  memset (buf, 0, sizeof (buf));
  fread (buf, strlen (line), 1, t);
  eputs ("buf=");
  eputs (buf);
  eputs ("\n");
  if (strcmp (buf, line))
    return 3;

  tmp = "bar";
  dump (tmp, contents);
  t = fopen (tmp, "w+");
  if (t <= 0)
    return 4;

  fwrite (end, strlen (end), 1, t);
  fseek (t, 0, SEEK_SET);
  memset (buf, 0, sizeof (buf));
  fread (buf, strlen (end), 1, t);
  eputs ("buf=");
  eputs (buf);
  eputs ("\n");
  if (strcmp (buf, end))
    return 5;

  fwrite (end, strlen (end), 1, t);

  fseek (t, 0, SEEK_SET);
  memset (buf, 0, sizeof (buf));
  fread (buf, strlen (end), 1, t);
  if (strcmp (buf, end))
    return 6;

  tmp = "baz";
  dump (tmp, contents);
  t = fopen (tmp, "a+");
  if (t <= 0)
    return 7;

  fwrite (end, strlen (end), 1, t);
  fseek (t, 0, SEEK_SET);
  memset (buf, 0, sizeof (buf));
  fread (buf, strlen (line), 1, t);
  eputs ("buf=");
  eputs (buf);
  eputs ("\n");
  if (strcmp (buf, line))
    return 8;

  fwrite (end, strlen (end), 1, t);

  fseek (t, 0, SEEK_SET);
  fread (buf, strlen (line), 1, t);
  eputs ("buf=");
  eputs (buf);
  eputs ("\n");
  if (strcmp (buf, line))
    return 9;

  return 0;
}

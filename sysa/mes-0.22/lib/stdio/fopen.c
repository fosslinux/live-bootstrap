/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2018 Jeremiah Orians <jeremiah@pdp10.guru>
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
#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

FILE *
fopen (char const *file_name, char const *opentype)
{
  if (__mes_debug ())
    {
      eputs ("fopen ");
      eputs (file_name);
      eputs (" ");
      eputs (opentype);
      eputs ("\n");
    }

  int fd;
  int mode = 0600;
  if ((opentype[0] == 'a' || !strcmp (opentype, "r+")) && !access (file_name, O_RDONLY))
    {
      int flags = O_RDWR;
      if (opentype[0] == 'a')
        flags |= O_APPEND;
      fd = _open3 (file_name, flags, mode);
    }
  else if (opentype[0] == 'w' || opentype[0] == 'a' || !strcmp (opentype, "r+"))
    {
      char *plus_p = strchr (opentype, '+');
      int flags = plus_p ? O_RDWR | O_CREAT : O_WRONLY | O_CREAT | O_TRUNC;
      fd = _open3 (file_name, flags, mode);
    }
  else
    fd = _open3 (file_name, O_RDONLY, 0);

  if (__mes_debug ())
    {
      eputs (" => fd=");
      eputs (itoa (fd));
      eputs ("\n");
    }

  if (!fd)
    {
      eputs (" ***MES LIB C*** fopen of stdin: signal me in band\n");
      assert (0);
    }
  if (fd < 0)
    fd = 0;
  return (FILE *) (long) fd;
}

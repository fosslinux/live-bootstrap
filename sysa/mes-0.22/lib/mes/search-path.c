/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

char *
search_path (char const *file_name)
{
  static char buf[256];
  char *path = getenv ("PATH");
  if (__mes_debug ())
    {
      eputs ("\n search-path: ");
      eputs (file_name);
      eputs ("\n");
    }
  while (*path)
    {
      char *end = strchr (path, ':');
      if (!end)
        end = strchr (path, '\0');
      strncpy (buf, path, end - path);
      buf[end - path] = 0;
      if (__mes_debug ())
        {
          eputs (" dir: ");
          eputs (buf);
          eputs ("\n");
        }
      if (buf[end - path] != '/')
        strcat (buf, "/");
      strcat (buf, file_name);
      if (!access (buf, X_OK))
        {
          if (__mes_debug ())
            {
              eputs (" found: ");
              eputs (buf);
              eputs ("\n");
            }
          return buf;
        }
      path = end + 1;
    }
  return 0;
}

/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
#include <stdarg.h>
#include <string.h>

int
execlp (char const *file_name, char const *arg, ...)
{
  va_list ap;
  int r;
  va_start (ap, arg);
  if (!strchr (file_name, '/'))
    file_name = search_path (file_name);
  if (__mes_debug () > 2)
    {
      eputs ("execlp ");
      eputs (file_name ? file_name : "0");
      eputs ("\n");
    }
  if (!file_name)
    {
      errno = ENOENT;
      return -1;
    }
  r = vexec (file_name, ap);
  va_end (ap);
  return r;
}

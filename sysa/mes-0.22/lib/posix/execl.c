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
#include <stdarg.h>
#include <unistd.h>

int
vexec (char const *file_name, va_list ap)
{
  char *arg = va_arg (ap, char *);
  char *argv[1000];             // POSIX minimum 4096
  int i = 0;

  argv[i++] = (char *) file_name;
  while (arg)
    {
      argv[i++] = arg;
      arg = va_arg (ap, char *);
      if (__mes_debug () > 2)
        {
          eputs ("arg[");
          eputs (itoa (i));
          eputs ("]: ");
          eputs (argv[i - 1]);
          eputs ("\n");
        }
    }
  argv[i] = 0;
  int r = execv (file_name, argv);
  va_end (ap);
  return r;
}

int
execl (char const *file_name, char const *arg, ...)
{
  va_list ap;
  int r;
  va_start (ap, arg);
  if (__mes_debug () > 2)
    {
      eputs ("execl ");
      eputs (file_name);
      eputs ("\n");
    }
  r = vexec (file_name, ap);
  va_end (ap);
  return r;
}

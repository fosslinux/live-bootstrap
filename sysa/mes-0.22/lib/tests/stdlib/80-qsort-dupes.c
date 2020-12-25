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
#include <string.h>

int
qsort_strcmp (void const *a, void const *b)
{
  return strcmp (*((char **) a), *((char **) b));
}

int
main ()
{
  char *list[3] = { "foo", "foo", 0 };
  oputs ("\nls:\n");
  qsort (list, 2, sizeof (char *), qsort_strcmp);
  for (int i = 0; i < 2; i++)
    {
      oputs (list[i]);
      oputs ("\n");
    }

  return 0;
}

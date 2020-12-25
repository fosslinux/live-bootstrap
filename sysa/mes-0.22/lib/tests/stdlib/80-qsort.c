/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
compare_int (void *a, void *b)
{
  eputs ("compare: ");
  eputs (itoa (*(int *) a));
  eputs (" <? ");
  eputs (itoa (*(int *) b));
  eputs (" => ");
  eputs (itoa (*(int *) a - *(int *) b));
  eputs ("\n");
  return *(int *) a - *(int *) b;
}

int
main ()
{
  int lst[6] = { 0, 5, 4, 3, 2, -1 };
  qsort (lst, 6, sizeof (int), compare_int);
  for (int i = 0; i < 6; i++)
    {
      eputs (itoa (i));
      eputs (":");
      eputs (itoa (lst[i]));
      eputs ("\n");
    }
  if (lst[0] != -1)
    return 1;
  if (lst[5] != 5)
    return 2;
  return 0;
}

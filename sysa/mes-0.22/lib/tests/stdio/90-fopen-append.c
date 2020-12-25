/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
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
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int
main ()
{
  FILE *test = fopen ("tmp", "a+");
  FILE *hold = fopen ("tmp", "r");
  int a;
  int b;
  int i = 1000;
  do
    {
      a = fgetc (test);
      b = fgetc (hold);
      fprintf (stdout, "%c == %c\n", a, b);
      if (i < 1000)
        {
          fflush (test);
          fputc ('a', test);
        }
      if (b == EOF)
        exit (EXIT_SUCCESS);
      i = i + 1;
    }
  while (a == b);
  fprintf (stderr, "OOOPS you were not supposed to get here\n");
  exit (EXIT_FAILURE);
}

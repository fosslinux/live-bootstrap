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
#include <string.h>
#include <mes/lib-mini.h>

char g_hello[] = "hello\n" "world\n";

char *g_hello2 = "hello\n" "world\n";

char g_hello3[] = {
  'h', 'e', 'l', 'l', 'o', '\n',
  'w', 'o', 'r', 'l', 'd', '\n',
  '\0',
}

;

int g_hello_int[] = { 0, 1, 2, 3, 4, 5 };

int
main (int argc)
{
  oputs ("0:"); oputs (g_hello); oputs ("\n");
  oputs ("2:"); oputs (g_hello2); oputs ("\n");
  oputs ("3:"); oputs (g_hello3); oputs ("\n");
  if (strcmp (g_hello, g_hello2))
    return 1;

  if (strcmp (g_hello, g_hello3))
    return 2;

  char hello[] =
    "hello\n"
    "world\n"
    ;

  char *hello2 =
    "hello\n"
    "world\n"
    ;

  oputs (hello);
  oputs (hello2);
  if (strcmp (hello, hello2))
    return 3;

  char hello3[] =
    {
      'h', 'e', 'l', 'l', 'o', '\n',
      'w', 'o', 'r', 'l', 'd', '\n',
      '\0',
    }
    ;

  oputs (hello3);
  if (strcmp (hello, hello3))
    return 4;

  if (g_hello_int[0])
    return 5;

  if (g_hello_int[1] != 1)
    return 6;

  int hello_int[] = {0, 1, 2, 3, 4, 5};
  if (hello_int[0])
    return 7;

  if (hello_int[1] != 1)
    return 8;

  return 0;
}

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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define memset xmemset
#define calloc xcalloc

void *
memset (void *s, int c, size_t n)
{
  char *p = s;
  while (n--)
    *p++ = c;
  return s;
}

void *
calloc (size_t nmemb, size_t size)
{
  size_t count = nmemb * size;
  void *p = malloc (count);
  memset (p, 0, count);
  return p;
}

/* {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'} */
char little_endian_table[16] =
  { 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46 };

char *
little_endian (unsigned value, char *c, int number_of_bytes)
{
  char table[16] =
    { 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46 };

  switch (number_of_bytes)
    {
    case 4:
      {
        c[6] = table[value >> 28];
        c[7] = table[(value >> 24) % 16];
      }
    case 3:
      {
        c[4] = table[(value >> 20) % 16];
        c[5] = table[(value >> 16) % 16];
      }
    case 2:
      {
        c[2] = table[(value >> 12) % 16];
        c[3] = table[(value >> 8) % 16];
      }
    case 1:
      {
        c[0] = table[(value >> 4) % 16];
        c[1] = table[value % 16];
        break;
      }
    default:
      return "invalid";
    }
  return c;
}

int
main ()
{
  char table[3] = { '0', '1', '2' };

  char *s;
  s = calloc (10, sizeof (char));
  eputs ("2=");
  eputs (little_endian (2, s, 1));
  eputs ("\n");
  if (strcmp (s, "02"))
    return 1;

  eputs ("8=");
  eputs (little_endian (8, s, 2));
  eputs ("\n");
  if (strcmp (s, "0800"))
    return 2;

  eputs ("16=");
  eputs (little_endian (16, s, 4));
  eputs ("\n");
  if (strcmp (s, "10000000"))
    return 3;

  return 0;
}

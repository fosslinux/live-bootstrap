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
#include <stdio.h>
#include <string.h>

struct file
{
  char buffer[1];
};

struct xfile
{
  char *buffer;
};

struct file file;
struct xfile xfile;

char *buffer = "aaaaaaaaaaaa";
char *bufzor = "bbbbbbbbbbbb";

int
main ()
{
  char *p;

  struct file *pfile = &file;
  strcpy (file.buffer, "0123456789\n");
  eputs (file.buffer);
  p = pfile->buffer;
  if (p[1] != '1')
    return 1;
  if (file.buffer[1] != '1')
    return 2;
  if (pfile->buffer[1] != '1')
    return 3;

  strcpy (file.buffer, "0123456789\n");
  eputs (file.buffer);
  memcpy (pfile->buffer + 1, " ", 1);
  eputs (file.buffer);
  if (p[1] != ' ')
    return 4;
  if (file.buffer[1] != ' ')
    return 5;
  if (pfile->buffer[1] != ' ')
    return 6;

  strcpy (file.buffer, "0123456789\n");
  eputs (file.buffer);
  p[2] = ' ';
  eputs (file.buffer);
  if (p[2] != ' ')
    return 7;
  if (file.buffer[2] != ' ')
    return 8;
  if (pfile->buffer[2] != ' ')
    return 9;

  strcpy (file.buffer, "0123456789\n");
  eputs (file.buffer);
  file.buffer[3] = ' ';
  eputs (file.buffer);
  if (p[3] != ' ')
    return 10;
  if (p[4] != '4')
    return 11;

  strcpy (file.buffer, "0123456789\n");
  eputs (file.buffer);
  pfile->buffer[4] = ' ';
  eputs (file.buffer);
  if (p[4] != ' ')
    return 12;
  if (p[5] != '5')
    return 13;

  xfile.buffer = &buffer;
  struct xfile *pxfile = &xfile;
  strcpy (xfile.buffer, "0123456789\n");
  eputs (xfile.buffer);
  p = pxfile->buffer;
  if (p[5] != '5')
    return 20;
  if (xfile.buffer[5] != '5')
    return 22;
  if (pxfile->buffer[5] != '5')
    return 23;

  strcpy (xfile.buffer, "0123456789\n");
  eputs (xfile.buffer);
  memcpy (pxfile->buffer + 5, " ", 1);
  eputs (xfile.buffer);
  if (p[5] != ' ')
    return 24;
  if (xfile.buffer[5] != ' ')
    return 25;
  if (pxfile->buffer[5] != ' ')
    return 26;

  strcpy (xfile.buffer, "0123456789\n");
  eputs (xfile.buffer);
  p[6] = ' ';
  eputs (xfile.buffer);
  if (p[6] != ' ')
    return 27;
  if (xfile.buffer[6] != ' ')
    return 28;
  if (pxfile->buffer[6] != ' ')
    return 29;

  strcpy (xfile.buffer, "0123456789\n");
  eputs (xfile.buffer);
  xfile.buffer[7] = ' ';
  eputs (xfile.buffer);
  if (p[7] != ' ')
    return 30;
  if (p[8] != '8')
    return 31;

  strcpy (xfile.buffer, "0123456789\n");
  eputs (xfile.buffer);
  pxfile->buffer[8] = ' ';
  eputs (xfile.buffer);
  if (p[8] != ' ')
    return 32;
  if (p[9] != '9')
    return 33;

  short *ps;
  ps = pfile->buffer;
  p = pfile->buffer;

  strcpy (file.buffer, "0123456789\n");
  eputs (file.buffer);
  memcpy (ps + 1, "  ", 2);
  eputs (file.buffer);
  eputs (itoa (ps[1]));
  eputs ("\n");
  eputs (itoa (((' ' << 8) + ' ')));
  eputs ("\n");
  if (ps[1] != ((' ' << 8) + ' '))
    return 40;
  if (p[4] != '4')
    return 41;

  strcpy (file.buffer, "0123456789\n");
  eputs (file.buffer);
  ps[2] = ((' ' << 8) + ' ');
  eputs (file.buffer);
  eputs (itoa (ps[2]));
  eputs ("\n");
  eputs (itoa (((' ' << 8) + ' ')));
  eputs ("\n");
  if (ps[2] != ((' ' << 8) + ' '))
    return 42;
  if (p[6] != '6')
    return 43;

  return 0;
}

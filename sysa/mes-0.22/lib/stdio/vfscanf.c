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
#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>

int
vfscanf (FILE * stream, char const *template, va_list ap)
{
  char p = fgetc (stream);
  char const *t = template;
  int count = 0;
  while (*t && p != EOF)
    if (*t != '%')
      {
        t++;
        p = fgetc (stream);
      }
    else
      {
        t++;
        char c = *t;
        if (c == 'l')
          c = *++t;
        switch (c)
          {
          case '%':
            {
              p = fgetc (stream);
              break;
            }
          case 'c':
            {
              char *c = va_arg (ap, char *);
              *c = p;
              p = fgetc (stream);
              count++;
              break;
            }
          case 'd':
          case 'i':
          case 'u':
            {
              int *d = va_arg (ap, int *);
              char buf[20];
              char *q = buf;
              if (p == '+' || p == '-')
                {
                  *q++ = p;
                  p = fgetc (stream);
                }
              while (isdigit (p))
                {
                  *q++ = p;
                  p = fgetc (stream);
                }
              ungetc (p, stream);
              *q = 0;
              q = buf;
              *d = abtol (&q, 10);
              count++;
              break;
            }
          case 'e':
          case 'f':
          case 'g':
          case 'E':
          case 'G':
            {
              float *f = va_arg (ap, float *);
              char buf[20];
              char *q = buf;
              if (p == '+' || p == '-')
                {
                  *q++ = p;
                  p = fgetc (stream);
                }
              while (isdigit (p))
                {
                  *q++ = p;
                  p = fgetc (stream);
                }
              ungetc (p, stream);
              *q = 0;
              q = buf;
              *f = strtod (q, &q);
              count++;
              break;
            }
          default:
            {
              eputs ("vsscanf: not supported: %:");
              eputc (c);
              eputs ("\n");
              t++;
              p = fgetc (stream);
            }
          }
        t++;
      }
  va_end (ap);
  return count;
}

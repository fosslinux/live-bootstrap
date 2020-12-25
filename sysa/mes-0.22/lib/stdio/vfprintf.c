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
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

int
vfprintf (FILE * f, char const *format, va_list ap)
{
  int fd = (long) f;
  char const *p = format;
  int count = 0;
  while (*p)
    if (*p != '%')
      {
        count++;
        fputc (*p++, f);
      }
    else
      {
        p++;
        char c = *p;
        int left_p = 0;
        int precision = -1;
        int width = -1;
        if (c == '-')
          {
            left_p = 1;
            c = *++p;
          }
        char pad = ' ';
        if (c == ' ')
          {
            pad = c;
            c = *p++;
          }
        if (c == '0')
          {
            pad = c;
            c = *p++;
          }
        if (c >= '0' && c <= '9')
          {
            width = abtol (&p, 10);
            c = *p;
          }
        else if (c == '*')
          {
            width = va_arg (ap, int);
            c = *++p;
          }
        if (c == '.')
          {
            c = *++p;
            if (c >= '0' && c <= '9')
              {
                precision = abtol (&p, 10);
                c = *p;
              }
            else if (c == '*')
              {
                precision = va_arg (ap, int);
                c = *++p;
              }
          }
        if (c == 'l')
          c = *++p;
        if (c == 'l')
          {
            eputs ("vfprintf: skipping second: l\n");
            c = *++p;
          }
        switch (c)
          {
          case '%':
            {
              fputc (*p, f);
              count++;
              break;
            }
          case 'c':
            {
              char _c;
              _c = va_arg (ap, long);
              fputc (_c, f);
              break;
            }
          case 'd':
          case 'i':
          case 'o':
          case 'u':
          case 'x':
          case 'X':
            {
              long d = va_arg (ap, long);
              int base = c == 'o' ? 8 : c == 'x' || c == 'X' ? 16 : 10;
              char *s = ntoab (d, base, c != 'u' && c != 'x' && c != 'X');
              if (c == 'X')
                strupr (s);
              int length = strlen (s);
              if (precision == -1)
                precision = length;
              if (!left_p)
                {
                  while (width-- > precision)
                    {
                      fputc (pad, f);
                      count++;
                    }
                  while (precision > length)
                    {
                      fputc ('0', f);
                      precision--;
                      width--;
                      count++;
                    }
                }
              while (*s)
                {
                  if (precision-- <= 0)
                    break;
                  width--;
                  fputc (*s++, f);
                  count++;
                }
              while (width > 0)
                {
                  width--;
                  fputc (pad, f);
                  count++;
                }
              break;
            }
          case 's':
            {
              char *s = va_arg (ap, char *);
              int length = strlen (s);
              if (precision == -1)
                precision = length;
              if (!left_p)
                {
                  while (width-- > precision)
                    {
                      fputc (pad, f);
                      count++;
                    }
                  while (precision > length)
                    {
                      fputc (' ', f);
                      precision--;
                      width--;
                      count++;
                    }
                }
              while (*s)
                {
                  if (precision-- <= 0)
                    break;
                  width--;
                  fputc (*s++, f);
                  count++;
                }
              while (width > 0)
                {
                  width--;
                  fputc (pad, f);
                  count++;
                }
              break;
            }
          case 'f':
          case 'e':
          case 'E':
          case 'g':
          case 'G':
            {
              double d = va_arg8 (ap, double);
              char *s = dtoab (d, 10, 1);
              if (c == 'E' || c == 'G')
                strupr (s);
              int length = strlen (s);
              if (precision == -1)
                precision = length;
              if (!left_p)
                {
                  while (width-- > precision)
                    {
                      fputc (pad, f);
                      count++;
                    }
                  while (precision > length)
                    {
                      fputc (' ', f);
                      precision--;
                      width--;
                      count++;
                    }
                }
              while (*s)
                {
                  if (precision-- <= 0)
                    break;
                  width--;
                  fputc (*s++, f);
                  count++;
                }
              while (width > 0)
                {
                  width--;
                  fputc (pad, f);
                  count++;
                }
              break;
            }
          case 'n':
            {
              int *n = va_arg (ap, int *);
              *n = count;
              break;
            }
          default:
            {
              eputs ("vfprintf: not supported: %:");
              eputc (c);
              eputs ("\n");
              p++;
            }
          }
        p++;
      }
  va_end (ap);
  return 0;
}

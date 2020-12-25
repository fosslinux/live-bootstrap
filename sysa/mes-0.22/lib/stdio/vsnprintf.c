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
#include <string.h>

int
vsnprintf (char *str, size_t size, char const *format, va_list ap)
{
  char const *p = format;
  int count = 0;
  char c;
  while (*p)
    if (*p != '%')
      {
        c = *p++;
        if (count < size)
          *str++ = c;
        count++;
      }
    else
      {
        p++;
        c = *p;
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
            width = va_arg (ap, long);
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
                precision = va_arg (ap, long);
                c = *++p;
              }
          }
        if (c == 'l')
          c = *++p;
        if (c == 'l')
          c = *++p;
        if (c == 'l')
          {
            eputs ("vsnprintf: skipping second: l\n");
            c = *++p;
          }
        switch (c)
          {
          case '%':
            {
              if (count < size)
                *str++ = *p;
              count++;
              break;
            }
          case 'c':
            {
              c = va_arg (ap, long);
              if (count < size)
                *str++ = c;
              count++;
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
                      if (count < size)
                        *str++ = pad;
                      count++;
                    }
                  while (precision > length)
                    {
                      if (count < size)
                        *str++ = '0';
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
                  c = *s++;
                  if (count < size)
                    *str++ = c;
                  count++;
                }
              while (width > 0)
                {
                  width--;
                  if (count < size)
                    *str++ = pad;
                  count++;
                }
              break;
            }
          case 's':
            {
              char *s = va_arg (ap, char *);
              int length = s ? strlen (s) : 0;
              if (precision == -1)
                precision = length;
              if (!left_p)
                {
                  while (width-- > precision)
                    {
                      if (count < size)
                        *str++ = pad;
                      count++;
                    }
                  while (width > length)
                    {
                      if (count < size)
                        *str++ = ' ';
                      precision--;
                      width--;
                      count++;
                    }
                }
              while (s && *s)
                {
                  if (precision-- <= 0)
                    break;
                  width--;
                  c = *s++;
                  if (count < size)
                    *str++ = c;
                  count++;
                }
              while (width > 0)
                {
                  width--;
                  if (count < size)
                    *str++ = pad;
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
                      if (count < size)
                        *str++ = pad;
                      count++;
                    }
                  while (precision > length)
                    {
                      if (count < size)
                        *str++ = ' ';
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
                  c = *s++;
                  if (count < size)
                    *str++ = c;
                  count++;
                }
              while (width > 0)
                {
                  width--;
                  if (count < size)
                    *str++ = pad;
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
              eputs ("vsnprintf: not supported: %:");
              eputc (c);
              eputs ("\n");
              p++;
            }
          }
        p++;
      }
  va_end (ap);
  if (count < size)
    *str = 0;
  return count;
}

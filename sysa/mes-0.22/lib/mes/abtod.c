/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

double
abtod (char const **p, int base)
{
  char const *s = *p;
  double d = 0;
  int sign_p = 0;
  if (!base)
    base = 10;
  double dbase = base;
  long i = abtol (&s, base);
  long f = 0;
  long e = 0;
  if (*s == '.')
    {
      s++;
      f = abtol (&s, base);
    }
  if (*s == 'e')
    {
      s++;
      e = abtol (&s, base);
    }
  d = i + f / dbase;
  if (e < 0)
    while (e++)
      d = d / dbase;
  while (e--)
    d = d * dbase;
  *p = s;
  return sign_p ? -d : d;
}

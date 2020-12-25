/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
#include <unistd.h>

char *
mktemp (char *template)
{
  char *p = strchr (template, '\0');
  int q = (long) template;
  *--p = ((unsigned char) (q >> 4)) % 26 + 'a';
  *--p = ((unsigned char) (q >> 8)) % 26 + 'a';
  *--p = ((unsigned char) (q >> 12)) % 26 + 'a';
  *--p = ((unsigned char) (q >> 16)) % 26 + 'a';
  *--p = ((unsigned char) (q >> 20)) % 26 + 'a';
  *--p = ((unsigned char) (q >> 24)) % 26 + 'a';
  return template;
}

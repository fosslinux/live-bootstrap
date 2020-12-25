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

#include <mes/lib.h>
#include <string.h>

char *sys_errlist[] = {
  "error 00",
  "error 01",
  "error 02",
  "error 03",
  "error 04",
  "error 05",
  "error 06",
  "error 07",
  "error 08",
  "error 09",
  "error 10",
  "error 11",
  "error 12",
  "error 13",
  "error 14",
  "error 15",
  "error 16",
  "error 17",
  "error 18",
  "error 19",
  "error 20",
  "error 21",
  "error 22",
  "error 23",
  "error 24",
  "error 25",
  "error 26",
  "error 27",
  "error 28",
  "error 29",
  "error 30",
  "error 31",
  "error 32",
  "error 33",
  "error 34",
  "error 35",
  "error 36",
  "error 37",
  "error 38",
  "error 39",
};

int sys_nerr = 39;

char *
strerror (int errnum)
{
  if (__mes_debug ())
    {
      eputs ("strerror errnum=");
      eputs (itoa (errnum));
      eputs ("\n");
    }
  if (errnum > 0 && errnum <= sys_nerr)
    return sys_errlist[errnum];
  return "sterror: unknown error";
}

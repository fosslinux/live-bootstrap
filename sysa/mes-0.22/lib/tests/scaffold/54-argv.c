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

#include <stdio.h>
#include <string.h>

int
main (int argc, char *argv[])
{
  oputs ("\n");
  oputs ("t: argv[0] == \"lib/tests/scaffold....\"\n");
  oputs ("argv0=");
  oputs (argv[0]);
  oputs ("\n");
  if (strcmp (argv[0], "lib/tests/scaffold/54-argv"))
    return 1;

  oputs ("t: *argv\"\n");
  oputs (*argv);
  oputs ("\n");

  if (argc != 5)
    return 2;

  if (strcmp (argv[1], "-s"))
    return 3;

  if (strcmp (argv[2], "--long"))
    return 4;

  return 0;
}

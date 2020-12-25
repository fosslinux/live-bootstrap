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
#include <setjmp.h>

int foo;
jmp_buf buf;

void
second ()
{
  if (foo++)
    exit (1);
  oputs ("second\n");           // prints
  longjmp (buf, 1);             // jumps back to where setjmp was called - making setjmp now return 1
  exit (1);
}

void
first ()
{
  second ();
  oputs ("first\n");            // does not print
  exit (2);
}

int
main ()
{
  if (!setjmp (buf))
    first ();                   // when executed, setjmp returned 0
  else                          // when longjmp jumps back, setjmp returns 1
    {
      oputs ("main\n");         // prints
      return 0;
    }

  return 3;
}

/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 *
 * This file is part of Mes.
 *
 * Mes is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 *
 * Mes is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Mes.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <gnu/hurd.h>

char **environ;
int __stdin;
int __stdout;
int __stderr;

int main ();
void _exit (int status);

void _hurd_start (void);

void
_start ()
{
  _hurd_start ();
  __stdin = 0;
  __stdout = 1;
  __stderr = 2;
  int r = main (__argc, __argv, environ);
  _exit (r);
  asm ("hlt");
}

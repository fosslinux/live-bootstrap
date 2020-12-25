/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2018 rain1
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

#include <stdio.h>

int
main (void)
{
  static void *lbls[] = { &&lbl_h, &&lbl_e, &&lbl_l, &&lbl_l, &&lbl_o, &&lbl_quit };
  static void **lbl = lbls;

  goto **lbl;

lbl_e:
  printf ("e");
  lbl++;
  goto **lbl;

lbl_o:
  printf ("o");
  lbl++;
  goto **lbl;
lbl_h:
  printf ("h");
  lbl++;
  goto **lbl;

lbl_l:
  printf ("l");
  lbl++;
  goto **lbl;

lbl_quit:
  puts ("");
  return 0;

}

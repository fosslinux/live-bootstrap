/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2019 Danny Milosavljevic <dannym@scratchpost.org>
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
#include <assert.h>
#include <stdint.h>
#include <limits.h>

/*#define LONG_MIN (-(~0UL>>1)-1)*/

typedef struct
{
  long quot;
  long rem;
} ldiv_t;

void __mesabi_div0(void)
{
  eputs(" ***MES C LIB*** divide by zero numerator=");
  eputs("\n");
  assert(0);
}

/* Compare gcc: __udivdi3 */
unsigned long __mesabi_uldiv(unsigned long a, unsigned long b, unsigned long* remainder)
{
  unsigned long tmp;
  if (!remainder)
    remainder = &tmp;
  *remainder = 0;
  if (b == 1)
    return a;
  else if (b == 0)
    __mesabi_div0();
  else
    {
      unsigned long x;
      for (x = 0; a >= b; a -= b)
        ++x;
      *remainder = a;
      return x;
    }
}

/* Note: Rounds towards zero.
   Maintainer: Be careful to satisfy quot * b + rem == a.
               That means that rem can be negative. */
ldiv_t ldiv(long a, long b)
{
  ldiv_t result;
  int negate_result = (a < 0) ^ (b < 0);
  assert(b != LONG_MIN);
  if (a != LONG_MIN)
    {
      int negative_a = (a < 0);
      if (negative_a)
        a = -a;
      if (b < 0)
        b = -b;
      result.quot = __mesabi_uldiv(a, b, &result.rem);
      if (negate_result)
        result.quot = -result.quot;
      if (negative_a)
        result.rem = -result.rem;
      return result;
    }
  else
    {
      result.rem = 0;
      if (b < 0)
        b = -b;
      if (b == 1)
        {
          result.quot = a;
          /* Since result.quot is already negative, don't negate it again. */
          negate_result = !negate_result;
        }
      else if (b == 0)
        __mesabi_div0();
      else
        {
          long x;
          for (x = 0; a <= -b; a += b)
            ++x;
          result.rem = a; /* negative */
          result.quot = x;
        }
      if (negate_result)
        result.quot = -result.quot;
      return result;
    }
}

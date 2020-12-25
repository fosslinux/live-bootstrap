/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include "mes/lib.h"
#include "mes/mes.h"

#include <assert.h>

#include <limits.h>
#include <string.h>

long MAX_STRING;

void
assert_max_string (size_t i, char const *msg, char *string)
{
  if (i > MAX_STRING)
    {
      eputs (msg);
      eputs (":string too long[");
      eputs (itoa (i));
      eputs ("]:");
      string[MAX_STRING - 1] = 0;
      eputs (string);
      error (cell_symbol_system_error, cell_f);
    }
}

char const *
list_to_cstring (SCM list, size_t * size)
{
  size_t i = 0;
  char *p = g_buf;
  while (list != cell_nil)
    {
      if (i > MAX_STRING)
        assert_max_string (i, "list_to_string", g_buf);
      g_buf[i++] = VALUE (car (list));
      list = cdr (list);
    }
  g_buf[i] = 0;
  *size = i;
  return g_buf;
}

size_t
bytes_cells (size_t length)
{
  return (1 + sizeof (long) + sizeof (long) + length + sizeof (SCM)) / sizeof (SCM);
}

SCM
make_bytes (char const *s, size_t length)
{
  size_t size = bytes_cells (length);
  SCM x = alloc (size);
  TYPE (x) = TBYTES;
  LENGTH (x) = length;
  char *p = (char *) &g_cells[x].cdr;
  if (!length)
    *(char *) p = 0;
  else
    memcpy (p, s, length + 1);
  return x;
}

SCM
make_string (char const *s, size_t length)
{
  if (length > MAX_STRING)
    assert_max_string (length, "make_string", (char *) s);
  SCM x = make_cell__ (TSTRING, length, 0);
  SCM v = make_bytes (s, length);
  CDR (x) = v;
  return x;
}

SCM
string_equal_p (SCM a, SCM b)   ///((name . "string=?"))
{
  if (!((TYPE (a) == TSTRING && TYPE (b) == TSTRING) || (TYPE (a) == TKEYWORD || TYPE (b) == TKEYWORD)))
    {
      eputs ("type a: ");
      eputs (itoa (TYPE (a)));
      eputs ("\n");
      eputs ("type b: ");
      eputs (itoa (TYPE (b)));
      eputs ("\n");
      eputs ("a= ");
      write_error_ (a);
      eputs ("\n");
      eputs ("b= ");
      write_error_ (b);
      eputs ("\n");
      assert ((TYPE (a) == TSTRING && TYPE (b) == TSTRING) || (TYPE (a) == TKEYWORD || TYPE (b) == TKEYWORD));
    }
  if (a == b
      || STRING (a) == STRING (b)
      || (!LENGTH (a) && !LENGTH (b))
      || (LENGTH (a) == LENGTH (b) && !memcmp (CSTRING (a), CSTRING (b), LENGTH (a))))
    return cell_t;
  return cell_f;
}

SCM
symbol_to_string (SCM symbol)
{
  return make_cell__ (TSTRING, CAR (symbol), CDR (symbol));
}

SCM
symbol_to_keyword (SCM symbol)
{
  return make_cell__ (TKEYWORD, CAR (symbol), CDR (symbol));
}

SCM
keyword_to_string (SCM keyword)
{
  return make_cell__ (TSTRING, CAR (keyword), CDR (keyword));
}

SCM
string_to_symbol (SCM string)
{
  SCM x = hash_ref (g_symbols, string, cell_f);
  if (x == cell_f)
    x = make_symbol (string);
  return x;
}

SCM
make_symbol (SCM string)
{
  SCM x = make_cell__ (TSYMBOL, LENGTH (string), STRING (string));
  hash_set_x (g_symbols, string, x);
  return x;
}

SCM
bytes_to_list (char const *s, size_t i)
{
  SCM p = cell_nil;
  while (i--)
    {
      int c = (0x100 + s[i]) % 0x100;
      p = cons (MAKE_CHAR (c), p);
    }
  return p;
}

SCM
cstring_to_list (char const *s)
{
  return bytes_to_list (s, strlen (s));
}

SCM
cstring_to_symbol (char const *s)
{
  SCM string = MAKE_STRING0 (s);
  return string_to_symbol (string);
}

SCM
string_to_list (SCM string)
{
  return bytes_to_list (CSTRING (string), LENGTH (string));
}

SCM
list_to_string (SCM list)
{
  size_t size;
  char const *s = list_to_cstring (list, &size);
  return make_string (s, size);
}

SCM
read_string (SCM port)          ///((arity . n))
{
  int fd = __stdin;
  if (TYPE (port) == TPAIR && TYPE (car (port)) == TNUMBER)
    __stdin = VALUE (CAR (port));
  int c = readchar ();
  size_t i = 0;
  while (c != -1)
    {
      if (i > MAX_STRING)
        assert_max_string (i, "read_string", g_buf);
      g_buf[i++] = c;
      c = readchar ();
    }
  g_buf[i] = 0;
  __stdin = fd;
  return make_string (g_buf, i);
}

SCM
string_append (SCM x)           ///((arity . n))
{
  char *p = g_buf;
  g_buf[0] = 0;
  size_t size = 0;
  while (x != cell_nil)
    {
      SCM string = CAR (x);
      assert (TYPE (string) == TSTRING);
      memcpy (p, CSTRING (string), LENGTH (string) + 1);
      p += LENGTH (string);
      size += LENGTH (string);
      if (size > MAX_STRING)
        assert_max_string (size, "string_append", g_buf);
      x = CDR (x);
    }
  return make_string (g_buf, size);
}

SCM
string_length (SCM string)
{
  assert (TYPE (string) == TSTRING);
  return MAKE_NUMBER (LENGTH (string));
}

SCM
string_ref (SCM str, SCM k)
{
  assert (TYPE (str) == TSTRING);
  assert (TYPE (k) == TNUMBER);
  size_t size = LENGTH (str);
  size_t i = VALUE (k);
  if (i > size)
    error (cell_symbol_system_error, cons (MAKE_STRING0 ("value out of range"), k));
  char const *p = CSTRING (str);
  return MAKE_CHAR (p[i]);
}

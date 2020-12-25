/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
#include <string.h>

int
hash_cstring (char const *s, long size)
{
  int hash = s[0] * 37;
  if (s[0] && s[1])
    hash = hash + s[1] * 43;
  assert (size);
  hash = hash % size;
  return hash;
}

int
hashq_ (SCM x, long size)
{
  if (TYPE (x) == TSPECIAL || TYPE (x) == TSYMBOL)
    return hash_cstring (CSTRING (x), size);    // FIXME: hash x directly
  error (cell_symbol_system_error, cons (MAKE_STRING0 ("hashq_: not a symbol"), x));
}

int
hash_ (SCM x, long size)
{
  if (TYPE (x) == TSTRING)
    return hash_cstring (CSTRING (x), size);
  assert (0);
  return hashq_ (x, size);
}

SCM
hashq (SCM x, SCM size)
{
  assert (0);
  return MAKE_NUMBER (hashq_ (x, VALUE (size)));
}

SCM
hash (SCM x, SCM size)
{
  assert (0);
  return MAKE_NUMBER (hash_ (x, VALUE (size)));
}

SCM
hashq_get_handle (SCM table, SCM key, SCM dflt)
{
  long size = VALUE (struct_ref_ (table, 3));
  unsigned hash = hashq_ (key, size);
  SCM buckets = struct_ref_ (table, 4);
  SCM bucket = vector_ref_ (buckets, hash);
  SCM x = cell_f;
  if (TYPE (dflt) == TPAIR)
    x = CAR (dflt);
  if (TYPE (bucket) == TPAIR)
    x = assq (key, bucket);
  return x;
}

SCM
hashq_ref (SCM table, SCM key, SCM dflt)
{
#if defined (INLINE)
  SCM x = hashq_get_handle (table, key, dflt);
#else
  long size = VALUE (struct_ref_ (table, 3));
  unsigned hash = hashq_ (key, size);
  SCM buckets = struct_ref_ (table, 4);
  SCM bucket = vector_ref_ (buckets, hash);
  SCM x = cell_f;
  if (TYPE (dflt) == TPAIR)
    x = CAR (dflt);
  if (TYPE (bucket) == TPAIR)
    x = assq (key, bucket);
#endif
  if (x != cell_f)
    x = CDR (x);
  return x;
}

SCM
hash_ref (SCM table, SCM key, SCM dflt)
{
  long size = VALUE (struct_ref_ (table, 3));
  unsigned hash = hash_ (key, size);
  SCM buckets = struct_ref_ (table, 4);
  SCM bucket = vector_ref_ (buckets, hash);
  SCM x = cell_f;
  if (TYPE (dflt) == TPAIR)
    x = CAR (dflt);
  if (TYPE (bucket) == TPAIR)
    {
      x = assoc (key, bucket);
      if (x != cell_f)
        x = CDR (x);
    }
  return x;
}

#if defined (INLINE)
#error INLINE
SCM
hash_set_x_ (SCM table, unsigned hash, SCM key, SCM value)
{
  SCM buckets = struct_ref_ (table, 4);
  SCM bucket = vector_ref_ (buckets, hash);
  if (TYPE (bucket) != TPAIR)
    bucket = cell_nil;
  bucket = acons (key, value, bucket);
  vector_set_x_ (buckets, hash, bucket);
  return value;
}
#endif

SCM
hashq_set_x (SCM table, SCM key, SCM value)
{
  long size = VALUE (struct_ref_ (table, 3));
  unsigned hash = hashq_ (key, size);
#if defined (INLINE)
  return hash_set_x_ (table, hash, key, value);
#else
  SCM buckets = struct_ref_ (table, 4);
  SCM bucket = vector_ref_ (buckets, hash);
  if (TYPE (bucket) != TPAIR)
    bucket = cell_nil;
  bucket = acons (key, value, bucket);
  vector_set_x_ (buckets, hash, bucket);
  return value;
#endif
}

SCM
hash_set_x (SCM table, SCM key, SCM value)
{
  long size = VALUE (struct_ref_ (table, 3));
  unsigned hash = hash_ (key, size);
#if defined (INLINE)
  return hash_set_x_ (table, hash, key, value);
#else
  SCM buckets = struct_ref_ (table, 4);
  SCM bucket = vector_ref_ (buckets, hash);
  if (TYPE (bucket) != TPAIR)
    bucket = cell_nil;
  bucket = acons (key, value, bucket);
  vector_set_x_ (buckets, hash, bucket);
  return value;
#endif
}

SCM
hash_table_printer (SCM table)
{
  fdputs ("#<", __stdout);
  display_ (struct_ref_ (table, 2));
  fdputc (' ', __stdout);
  fdputs ("size: ", __stdout);
  display_ (struct_ref_ (table, 3));
  fdputc (' ', __stdout);
  SCM buckets = struct_ref_ (table, 4);
  fdputs ("buckets: ", __stdout);
  for (int i = 0; i < LENGTH (buckets); i++)
    {
      SCM e = vector_ref_ (buckets, i);
      if (e != cell_unspecified)
        {
          fdputc ('[', __stdout);
          while (TYPE (e) == TPAIR)
            {
              write_ (CAAR (e));
              e = CDR (e);
              if (TYPE (e) == TPAIR)
                fdputc (' ', __stdout);
            }
          fdputs ("]\n  ", __stdout);
        }
    }
  fdputc ('>', __stdout);
}

SCM
make_hashq_type ()              ///((internal))
{
  SCM record_type = cell_symbol_record_type;    // FIXME
  SCM fields = cell_nil;
  fields = cons (cell_symbol_buckets, fields);
  fields = cons (cell_symbol_size, fields);
  fields = cons (fields, cell_nil);
  fields = cons (cell_symbol_hashq_table, fields);
  return make_struct (record_type, fields, cell_unspecified);
}

SCM
make_hash_table_ (long size)
{
  if (!size)
    size = 100;
  SCM hashq_type = make_hashq_type ();

  SCM buckets = make_vector__ (size);
  SCM values = cell_nil;
  values = cons (buckets, values);
  values = cons (MAKE_NUMBER (size), values);
  values = cons (cell_symbol_hashq_table, values);
  //FIXME: symbol/printer return make_struct (hashq_type, values, cstring_to_symbol ("hash-table-printer");
  return make_struct (hashq_type, values, cell_unspecified);
}

SCM
make_hash_table (SCM x)
{
  long size = 0;
  if (TYPE (x) == TPAIR)
    {
      assert (TYPE (x) == TNUMBER);
      size = VALUE (x);
    }
  return make_hash_table_ (size);
}

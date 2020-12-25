/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
#include <string.h>

#if !__MESC__
#define __READ_BUFFER_MAX 128
int __read_buffer_max;
#else /* FIXME: We want bin/mes-mescc's x86-linux sha256sum to stay the same. */
#define __READ_BUFFER_MAX 100
#define __read_buffer_max 100
#endif

struct __read_buffer
{
  ssize_t size;
  char string[__READ_BUFFER_MAX];
};

struct __read_buffer *__read_cache = 0;

void
__buffered_read_init (int filedes)
{
  if (!__read_cache)
    {
      __read_cache = (struct __read_buffer *) malloc (sizeof (struct __read_buffer) * __FILEDES_MAX);
#if !__MESC__
      __read_buffer_max = __READ_BUFFER_MAX;
      char *p = getenv ("MES_READ_BUFFER");
      if (p)
        {
          __read_buffer_max = atoi (p);
          if (__read_buffer_max < 0)
            __read_buffer_max = 0;
          if (__read_buffer_max > __READ_BUFFER_MAX)
            __read_buffer_max = __READ_BUFFER_MAX;
        }
#endif
    }
}

size_t
__buffered_read_clear (int filedes)
{
  __buffered_read_init (filedes);
  size_t size = __read_cache[filedes].size;
  __read_cache[filedes].size = 0;
  return size;
}

ssize_t
__buffered_read (int filedes, void *buffer, size_t size)
{
  size_t todo = size;
  __buffered_read_init (filedes);
  struct __read_buffer *cache = &__read_cache[filedes];
  char *p = buffer;
  if (!cache->size && size > __read_buffer_max)
    return _read (filedes, buffer, size);
  while (cache->size > 0 && todo)
    {
      todo--;
      *p++ = cache->string[__read_buffer_max - cache->size--];
    }
  if (todo)
    {
#if !__MESC__
      if (todo > __read_buffer_max)
        return size - todo + _read (filedes, p, todo);
      if (__mes_debug () > 4)
        {
          eputs ("__buffered_read: ");
          eputs (itoa (__read_buffer_max));
          eputs ("\n");
        }
#endif
      ssize_t bytes = _read (filedes, cache->string, __read_buffer_max);
      if (bytes < 0)
        return -1;
      if (bytes)
        {
          cache->size = bytes;
          if (bytes < __read_buffer_max)
            memmove (cache->string + __read_buffer_max - bytes, cache->string, bytes);
          return size - todo + __buffered_read (filedes, p, todo);
        }
    }
  return size - todo;
}

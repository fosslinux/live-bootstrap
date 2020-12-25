/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <mes/lib.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

void
add (void *ptab, int *nb_ptr, void *data)
{
  int nb, nb_alloc;
  int **pp;

  nb = *nb_ptr;
  pp = *(void ***) ptab;
  /* every power of two we double array size */
  if ((nb & (nb - 1)) == 0)
    {
      if (!nb)
        nb_alloc = 1;
      else
        nb_alloc = nb * 2;
      pp = realloc (pp, nb_alloc * sizeof (void *));
      *(void ***) ptab = pp;
    }
  pp[nb++] = data;
  *nb_ptr = nb;
}

typedef struct file4
{
  char name[4];
} file4_struct;

typedef struct file12
{
  int foo;
  int bar;
  char name[4];
} file12_struct;

//#define file file4
#define file file12

struct state
{
  int bla;
  char **paths;
  int path_count;
  struct file **files;
  //file_struct **files;
  int file_count;
};

struct state g_s;

int
main ()
{
  struct state *s = &g_s;

  char *file_name = "file-name";
  struct file *file;

  file = malloc (sizeof (struct file) + strlen (file_name));
  strcpy (file->name, file_name);
  add (&s->files, &s->file_count, file);

  char *path_name = "foo:bar:baz";
  add (&s->paths, &s->path_count, path_name);

  if (strcmp (*s->paths, path_name))
    return 1;

  eputs ("&PATHS=");
  eputs (itoa (&s->paths));
  eputs ("\n");
  eputs ("&FILES=");
  eputs (itoa (&s->files));
  eputs ("\n");

  // struct file *fs;
  // eputs ("foo\n");
  // fs = s->files[0];
  struct file *fs = s->files[0];
  eputs ("add s=   ");
  eputs (itoa (s));
  eputs ("\n");
  eputs ("add fs=  ");
  eputs (itoa (fs));
  eputs ("\n");
  eputs ("&fs->[0]=");
  eputs (itoa (fs->name));
  eputs ("\n");
  eputs ("fs->name=");
  eputs (fs->name);
  eputs ("\n");

  eputs ("ps=      ");
  eputs (itoa (s->paths));
  eputs ("\n");
  eputs ("*ps      ");
  eputs (*s->paths);
  eputs ("\n");

  if (strcmp (fs->name, file_name))
    return 2;

  eputs ("&fs->[0]=");
  eputs (itoa (fs->name));
  eputs ("\n");
  eputs ("fs->name=");
  eputs (fs->name);
  eputs ("\n");

  eputs ("ps=      ");
  eputs (itoa (s->paths));
  eputs ("\n");
  eputs ("*ps      ");
  eputs (*s->paths);
  eputs ("\n");


  file = malloc (sizeof (struct file) + strlen (file_name));
  file_name = "hallo";
  strcpy (file->name, file_name);
  add (&s->files, &s->file_count, file);

  struct file **pf = s->files;
  fs = pf[0];
  eputs ("\n");
  eputs ("&fs0*=    ");
  eputs (itoa (&pf[0]));
  eputs ("\n");

  eputs ("fs0*=     ");
  eputs (itoa (fs));
  eputs ("\n");
  fs = s->files[0];
  eputs ("fs0*=     ");
  eputs (itoa (fs));
  eputs ("\n");
  eputs ("\n");

  pf = s->files;
  fs = pf[1];
  eputs ("&fs1*=    ");
  eputs (itoa (&pf[1]));
  eputs ("\n");
  eputs ("fs1*=     ");
  eputs (itoa (fs));
  eputs ("\n");
  fs = s->files[1];
  eputs ("fs1*=     ");
  eputs (itoa (fs));
  eputs ("\n");
  eputs ("\n");
  if (strcmp (fs->name, file_name))
    return 3;

  fs = g_s.files[0];
  eputs ("gfs0*=    ");
  eputs (itoa (fs));
  eputs ("\n");
  fs = g_s.files[1];
  eputs ("gfs1*=    ");
  eputs (itoa (fs));
  eputs ("\n");
  eputs ("\n");
  if (strcmp (fs->name, file_name))
    return 3;


  return 0;
}

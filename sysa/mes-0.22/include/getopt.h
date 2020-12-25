/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright (C) 1989, 1990, 1991, 1992 Free Software Foundation, Inc.
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
#ifndef __MES_GETOPT_H
#define __MES_GETOPT_H 1

#if SYSTEM_LIBC
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#undef __MES_GETOPT_H
#include_next <getopt.h>

#else // ! SYSTEM_LIBC
#include <endian.h>
int isdigit (int);
int isxdigit (int);

char *optarg;
int optind;
int opterr;
struct option
{
  char const *name;
  int has_arg;
  int *flag;
  int val;
};

enum _argtype
{
  no_argument,
  required_argument,
  optional_argument
};

int getopt (int argc, char *const *argv, char const *options);
int getopt_long (int argc, char *const *argv, char const *options,
                 struct option const *long_options, int *opt_index);
int getopt_long_only (int argc, char *const *argv, char const *options,
                      struct option const *long_options, int *opt_index);

#endif // ! SYSTEM_LIBC

#endif // __MES_GETOPT_H

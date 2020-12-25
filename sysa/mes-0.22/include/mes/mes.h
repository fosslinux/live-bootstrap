/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#ifndef __MES_MES_H
#define __MES_MES_H

#include <sys/types.h>

typedef long SCM;

struct scm
{
  long type;
  SCM car;
  SCM cdr;
};

// mes
extern int g_debug;
extern char *g_buf;
extern SCM g_continuations;
extern SCM g_symbols;
extern SCM g_symbol_max;

// a/env
extern SCM r0;
// param 1
extern SCM r1;
// save 2
extern SCM r2;
// continuation
extern SCM r3;
// current-module
extern SCM m0;
// macro
extern SCM g_macros;
extern SCM g_ports;

// gc
extern long ARENA_SIZE;
extern long MAX_ARENA_SIZE;
extern long STACK_SIZE;
extern long JAM_SIZE;
extern long GC_SAFETY;
extern long MAX_STRING;
extern char *g_arena;
extern long g_free;
extern SCM g_stack;
extern SCM *g_stack_array;
extern struct scm *g_cells;
extern struct scm *g_news;

SCM alloc (long n);
SCM apply (SCM f, SCM x, SCM a);
SCM apply_builtin (SCM fn, SCM x);
SCM cstring_to_list (char const *s);
SCM cstring_to_symbol (char const *s);
SCM display_ (SCM x);
SCM fdisplay_ (SCM, int, int);
SCM gc_init ();
SCM gc_peek_frame ();
SCM gc_pop_frame ();
SCM gc_push_frame ();
SCM init_time (SCM a);
SCM make_bytes (char const *s, size_t length);
SCM make_cell__ (long type, SCM car, SCM cdr);
SCM make_hash_table_ (long size);
SCM make_hashq_type ();
SCM make_initial_module (SCM a);
SCM make_string (char const *s, size_t length);
SCM make_vector__ (long k);
SCM read_input_file_env (SCM);
SCM string_equal_p (SCM a, SCM b);
SCM struct_ref_ (SCM x, long i);
SCM struct_set_x_ (SCM x, long i, SCM e);
SCM vector_ref_ (SCM x, long i);
SCM vector_set_x_ (SCM x, long i, SCM e);
int peekchar ();
int readchar ();
int unreadchar ();
long length__ (SCM x);
size_t bytes_cells (size_t length);
void assert_max_string (size_t i, char const *msg, char *string);

#include "mes/builtins.h"
#include "mes/constants.h"
#include "mes/macros.h"

#endif //__MES_MES_H

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

#ifndef __MES_BUILTINS_H
#define __MES_BUILTINS_H

// src/gc.mes
SCM gc_check ();
SCM gc ();
// src/hash.mes
SCM hashq (SCM x, SCM size);
SCM hash (SCM x, SCM size);
SCM hashq_get_handle (SCM table, SCM key, SCM dflt);
SCM hashq_ref (SCM table, SCM key, SCM dflt);
SCM hash_ref (SCM table, SCM key, SCM dflt);
SCM hashq_set_x (SCM table, SCM key, SCM value);
SCM hash_set_x (SCM table, SCM key, SCM value);
SCM hash_table_printer (SCM table);
SCM make_hash_table (SCM x);
// src/lib.mes
SCM procedure_name_ (SCM x);
SCM display_ (SCM x);
SCM display_error_ (SCM x);
SCM display_port_ (SCM x, SCM p);
SCM write_ (SCM x);
SCM write_error_ (SCM x);
SCM write_port_ (SCM x, SCM p);
SCM exit_ (SCM x);
SCM frame_printer (SCM frame);
SCM make_stack (SCM stack);
SCM stack_length (SCM stack);
SCM stack_ref (SCM stack, SCM index);
SCM xassq (SCM x, SCM a);
SCM memq (SCM x, SCM a);
SCM equal2_p (SCM a, SCM b);
SCM last_pair (SCM x);
SCM pair_p (SCM x);
// src/math.mes
SCM greater_p (SCM x);
SCM less_p (SCM x);
SCM is_p (SCM x);
SCM minus (SCM x);
SCM plus (SCM x);
SCM divide (SCM x);
SCM modulo (SCM a, SCM b);
SCM multiply (SCM x);
SCM logand (SCM x);
SCM logior (SCM x);
SCM lognot (SCM x);
SCM logxor (SCM x);
SCM ash (SCM n, SCM count);
// src/mes.mes
SCM make_cell_ (SCM type, SCM car, SCM cdr);
SCM type_ (SCM x);
SCM car_ (SCM x);
SCM cdr_ (SCM x);
SCM arity_ (SCM x);
SCM cons (SCM x, SCM y);
SCM car (SCM x);
SCM cdr (SCM x);
SCM list (SCM x);
SCM null_p (SCM x);
SCM eq_p (SCM x, SCM y);
SCM values (SCM x);
SCM acons (SCM key, SCM value, SCM alist);
SCM length (SCM x);
SCM error (SCM key, SCM x);
SCM append2 (SCM x, SCM y);
SCM append_reverse (SCM x, SCM y);
SCM reverse_x_ (SCM x, SCM t);
SCM pairlis (SCM x, SCM y, SCM a);
SCM call (SCM fn, SCM x);
SCM assq (SCM x, SCM a);
SCM assoc (SCM x, SCM a);
SCM set_car_x (SCM x, SCM e);
SCM set_cdr_x (SCM x, SCM e);
SCM set_env_x (SCM x, SCM e, SCM a);
SCM macro_get_handle (SCM name);
SCM add_formals (SCM formals, SCM x);
SCM eval_apply ();
SCM make_builtin_type ();
SCM make_builtin (SCM builtin_type, SCM name, SCM arity, SCM function);
SCM builtin_arity (SCM builtin);
SCM builtin_p (SCM x);
SCM builtin_printer (SCM builtin);
// CONSTANT cell_nil 1
#define cell_nil 1
// CONSTANT cell_f 2
#define cell_f 2
// CONSTANT cell_t 3
#define cell_t 3
// CONSTANT cell_dot 4
#define cell_dot 4
// CONSTANT cell_arrow 5
#define cell_arrow 5
// CONSTANT cell_undefined 6
#define cell_undefined 6
// CONSTANT cell_unspecified 7
#define cell_unspecified 7
// CONSTANT cell_closure 8
#define cell_closure 8
// CONSTANT cell_circular 9
#define cell_circular 9
// CONSTANT cell_begin 10
#define cell_begin 10
// CONSTANT cell_call_with_current_continuation 11
#define cell_call_with_current_continuation 11
// CONSTANT cell_vm_apply 12
#define cell_vm_apply 12
// CONSTANT cell_vm_apply2 13
#define cell_vm_apply2 13
// CONSTANT cell_vm_begin 14
#define cell_vm_begin 14
// CONSTANT cell_vm_begin_eval 15
#define cell_vm_begin_eval 15
// CONSTANT cell_vm_begin_expand 16
#define cell_vm_begin_expand 16
// CONSTANT cell_vm_begin_expand_eval 17
#define cell_vm_begin_expand_eval 17
// CONSTANT cell_vm_begin_expand_macro 18
#define cell_vm_begin_expand_macro 18
// CONSTANT cell_vm_begin_expand_primitive_load 19
#define cell_vm_begin_expand_primitive_load 19
// CONSTANT cell_vm_begin_primitive_load 20
#define cell_vm_begin_primitive_load 20
// CONSTANT cell_vm_begin_read_input_file 21
#define cell_vm_begin_read_input_file 21
// CONSTANT cell_vm_call_with_current_continuation2 22
#define cell_vm_call_with_current_continuation2 22
// CONSTANT cell_vm_call_with_values2 23
#define cell_vm_call_with_values2 23
// CONSTANT cell_vm_eval 24
#define cell_vm_eval 24
// CONSTANT cell_vm_eval2 25
#define cell_vm_eval2 25
// CONSTANT cell_vm_eval_check_func 26
#define cell_vm_eval_check_func 26
// CONSTANT cell_vm_eval_define 27
#define cell_vm_eval_define 27
// CONSTANT cell_vm_eval_macro_expand_eval 28
#define cell_vm_eval_macro_expand_eval 28
// CONSTANT cell_vm_eval_macro_expand_expand 29
#define cell_vm_eval_macro_expand_expand 29
// CONSTANT cell_vm_eval_pmatch_car 30
#define cell_vm_eval_pmatch_car 30
// CONSTANT cell_vm_eval_pmatch_cdr 31
#define cell_vm_eval_pmatch_cdr 31
// CONSTANT cell_vm_eval_set_x 32
#define cell_vm_eval_set_x 32
// CONSTANT cell_vm_evlis 33
#define cell_vm_evlis 33
// CONSTANT cell_vm_evlis2 34
#define cell_vm_evlis2 34
// CONSTANT cell_vm_evlis3 35
#define cell_vm_evlis3 35
// CONSTANT cell_vm_if 36
#define cell_vm_if 36
// CONSTANT cell_vm_if_expr 37
#define cell_vm_if_expr 37
// CONSTANT cell_vm_macro_expand 38
#define cell_vm_macro_expand 38
// CONSTANT cell_vm_macro_expand_car 39
#define cell_vm_macro_expand_car 39
// CONSTANT cell_vm_macro_expand_cdr 40
#define cell_vm_macro_expand_cdr 40
// CONSTANT cell_vm_macro_expand_define 41
#define cell_vm_macro_expand_define 41
// CONSTANT cell_vm_macro_expand_define_macro 42
#define cell_vm_macro_expand_define_macro 42
// CONSTANT cell_vm_macro_expand_lambda 43
#define cell_vm_macro_expand_lambda 43
// CONSTANT cell_vm_macro_expand_set_x 44
#define cell_vm_macro_expand_set_x 44
// CONSTANT cell_vm_return 45
#define cell_vm_return 45
// CONSTANT cell_symbol_dot 46
#define cell_symbol_dot 46
// CONSTANT cell_symbol_lambda 47
#define cell_symbol_lambda 47
// CONSTANT cell_symbol_begin 48
#define cell_symbol_begin 48
// CONSTANT cell_symbol_if 49
#define cell_symbol_if 49
// CONSTANT cell_symbol_quote 50
#define cell_symbol_quote 50
// CONSTANT cell_symbol_define 51
#define cell_symbol_define 51
// CONSTANT cell_symbol_define_macro 52
#define cell_symbol_define_macro 52
// CONSTANT cell_symbol_quasiquote 53
#define cell_symbol_quasiquote 53
// CONSTANT cell_symbol_unquote 54
#define cell_symbol_unquote 54
// CONSTANT cell_symbol_unquote_splicing 55
#define cell_symbol_unquote_splicing 55
// CONSTANT cell_symbol_syntax 56
#define cell_symbol_syntax 56
// CONSTANT cell_symbol_quasisyntax 57
#define cell_symbol_quasisyntax 57
// CONSTANT cell_symbol_unsyntax 58
#define cell_symbol_unsyntax 58
// CONSTANT cell_symbol_unsyntax_splicing 59
#define cell_symbol_unsyntax_splicing 59
// CONSTANT cell_symbol_set_x 60
#define cell_symbol_set_x 60
// CONSTANT cell_symbol_sc_expand 61
#define cell_symbol_sc_expand 61
// CONSTANT cell_symbol_macro_expand 62
#define cell_symbol_macro_expand 62
// CONSTANT cell_symbol_portable_macro_expand 63
#define cell_symbol_portable_macro_expand 63
// CONSTANT cell_symbol_sc_expander_alist 64
#define cell_symbol_sc_expander_alist 64
// CONSTANT cell_symbol_call_with_values 65
#define cell_symbol_call_with_values 65
// CONSTANT cell_symbol_call_with_current_continuation 66
#define cell_symbol_call_with_current_continuation 66
// CONSTANT cell_symbol_boot_module 67
#define cell_symbol_boot_module 67
// CONSTANT cell_symbol_current_module 68
#define cell_symbol_current_module 68
// CONSTANT cell_symbol_primitive_load 69
#define cell_symbol_primitive_load 69
// CONSTANT cell_symbol_read_input_file 70
#define cell_symbol_read_input_file 70
// CONSTANT cell_symbol_write 71
#define cell_symbol_write 71
// CONSTANT cell_symbol_display 72
#define cell_symbol_display 72
// CONSTANT cell_symbol_car 73
#define cell_symbol_car 73
// CONSTANT cell_symbol_cdr 74
#define cell_symbol_cdr 74
// CONSTANT cell_symbol_not_a_number 75
#define cell_symbol_not_a_number 75
// CONSTANT cell_symbol_not_a_pair 76
#define cell_symbol_not_a_pair 76
// CONSTANT cell_symbol_system_error 77
#define cell_symbol_system_error 77
// CONSTANT cell_symbol_throw 78
#define cell_symbol_throw 78
// CONSTANT cell_symbol_unbound_variable 79
#define cell_symbol_unbound_variable 79
// CONSTANT cell_symbol_wrong_number_of_args 80
#define cell_symbol_wrong_number_of_args 80
// CONSTANT cell_symbol_wrong_type_arg 81
#define cell_symbol_wrong_type_arg 81
// CONSTANT cell_symbol_buckets 82
#define cell_symbol_buckets 82
// CONSTANT cell_symbol_builtin 83
#define cell_symbol_builtin 83
// CONSTANT cell_symbol_frame 84
#define cell_symbol_frame 84
// CONSTANT cell_symbol_hashq_table 85
#define cell_symbol_hashq_table 85
// CONSTANT cell_symbol_module 86
#define cell_symbol_module 86
// CONSTANT cell_symbol_procedure 87
#define cell_symbol_procedure 87
// CONSTANT cell_symbol_record_type 88
#define cell_symbol_record_type 88
// CONSTANT cell_symbol_size 89
#define cell_symbol_size 89
// CONSTANT cell_symbol_stack 90
#define cell_symbol_stack 90
// CONSTANT cell_symbol_argv 91
#define cell_symbol_argv 91
// CONSTANT cell_symbol_mes_prefix 92
#define cell_symbol_mes_prefix 92
// CONSTANT cell_symbol_mes_version 93
#define cell_symbol_mes_version 93
// CONSTANT cell_symbol_internal_time_units_per_second 94
#define cell_symbol_internal_time_units_per_second 94
// CONSTANT cell_symbol_compiler 95
#define cell_symbol_compiler 95
// CONSTANT cell_symbol_arch 96
#define cell_symbol_arch 96
// CONSTANT cell_symbol_pmatch_car 97
#define cell_symbol_pmatch_car 97
// CONSTANT cell_symbol_pmatch_cdr 98
#define cell_symbol_pmatch_cdr 98
// CONSTANT cell_type_bytes 99
#define cell_type_bytes 99
// CONSTANT cell_type_char 100
#define cell_type_char 100
// CONSTANT cell_type_closure 101
#define cell_type_closure 101
// CONSTANT cell_type_continuation 102
#define cell_type_continuation 102
// CONSTANT cell_type_function 103
#define cell_type_function 103
// CONSTANT cell_type_keyword 104
#define cell_type_keyword 104
// CONSTANT cell_type_macro 105
#define cell_type_macro 105
// CONSTANT cell_type_number 106
#define cell_type_number 106
// CONSTANT cell_type_pair 107
#define cell_type_pair 107
// CONSTANT cell_type_port 108
#define cell_type_port 108
// CONSTANT cell_type_ref 109
#define cell_type_ref 109
// CONSTANT cell_type_special 110
#define cell_type_special 110
// CONSTANT cell_type_string 111
#define cell_type_string 111
// CONSTANT cell_type_struct 112
#define cell_type_struct 112
// CONSTANT cell_type_symbol 113
#define cell_type_symbol 113
// CONSTANT cell_type_values 114
#define cell_type_values 114
// CONSTANT cell_type_variable 115
#define cell_type_variable 115
// CONSTANT cell_type_vector 116
#define cell_type_vector 116
// CONSTANT cell_type_broken_heart 117
#define cell_type_broken_heart 117
// CONSTANT cell_symbol_test 118
#define cell_symbol_test 118
// src/module.mes
SCM make_module_type ();
SCM module_printer (SCM module);
SCM module_variable (SCM module, SCM name);
SCM module_ref (SCM module, SCM name);
SCM module_define_x (SCM module, SCM name, SCM value);
// src/posix.mes
SCM peek_byte ();
SCM read_byte ();
SCM unread_byte (SCM i);
SCM peek_char ();
SCM read_char (SCM port);
SCM unread_char (SCM i);
SCM write_char (SCM i);
SCM write_byte (SCM x);
SCM getenv_ (SCM s);
SCM setenv_ (SCM s, SCM v);
SCM access_p (SCM file_name, SCM mode);
SCM current_input_port ();
SCM open_input_file (SCM file_name);
SCM open_input_string (SCM string);
SCM set_current_input_port (SCM port);
SCM current_output_port ();
SCM current_error_port ();
SCM open_output_file (SCM x);
SCM set_current_output_port (SCM port);
SCM set_current_error_port (SCM port);
SCM force_output (SCM p);
SCM chmod_ (SCM file_name, SCM mode);
SCM isatty_p (SCM port);
SCM primitive_fork ();
SCM execl_ (SCM file_name, SCM args);
SCM waitpid_ (SCM pid, SCM options);
SCM current_time ();
SCM gettimeofday_ ();
SCM get_internal_run_time ();
SCM getcwd_ ();
SCM dup_ (SCM port);
SCM dup2_ (SCM old, SCM new);
SCM delete_file (SCM file_name);
// src/reader.mes
SCM read_input_file_env_ (SCM e, SCM a);
SCM read_input_file_env (SCM a);
SCM read_env (SCM a);
SCM reader_read_sexp (SCM c, SCM s, SCM a);
SCM reader_read_character ();
SCM reader_read_binary ();
SCM reader_read_octal ();
SCM reader_read_hex ();
SCM reader_read_string ();
// src/strings.mes
SCM string_equal_p (SCM a, SCM b);
SCM symbol_to_string (SCM symbol);
SCM symbol_to_keyword (SCM symbol);
SCM keyword_to_string (SCM keyword);
SCM string_to_symbol (SCM string);
SCM make_symbol (SCM string);
SCM string_to_list (SCM string);
SCM list_to_string (SCM list);
SCM read_string (SCM port);
SCM string_append (SCM x);
SCM string_length (SCM string);
SCM string_ref (SCM str, SCM k);
// src/struct.mes
SCM make_struct (SCM type, SCM fields, SCM printer);
SCM struct_length (SCM x);
SCM struct_ref (SCM x, SCM i);
SCM struct_set_x (SCM x, SCM i, SCM e);
// src/vector.mes
SCM make_vector_ (SCM n);
SCM vector_length (SCM x);
SCM vector_ref (SCM x, SCM i);
SCM vector_entry (SCM x);
SCM vector_set_x (SCM x, SCM i, SCM e);
SCM list_to_vector (SCM x);
SCM vector_to_list (SCM v);

#endif //__MES_BUILTINS_H

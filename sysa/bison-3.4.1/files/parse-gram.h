// SPDX-License-Identifier: GPL-3.0-or-later

/*
 * This file is based on parse-gram.y from GNU Bison 3.4.1. It is the
 * header for an implementation of a subset of the grammar described
 * by parse-gram.y, just enough to provide a bootstrapping path for
 * Bison.
 *
 * Copyright (c) 2020, Giovanni Mascellani <gio@debian.org>
 *
 * The copyright notice of the original file follows. This file is
 * distributed under the same license and with the same conditions.
 */

/* Bison Grammar Parser                             -*- C -*-

   Copyright (C) 2002-2015, 2018-2019 Free Software Foundation, Inc.

   This file is part of Bison, the GNU Compiler Compiler.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

#ifndef PARSE_GRAM_H
#define PARSE_GRAM_H

/* %code requires  */

  #include "symlist.h"
  #include "symtab.h"

  typedef enum
  {
    param_none   = 0,
    param_lex    = 1 << 0,
    param_parse  = 1 << 1,
    param_both   = param_lex | param_parse
  } param_type;

  #include "muscle-tab.h"
  typedef struct
  {
    char const *chars;
    muscle_kind kind;
  } value_type;


enum gram_tokentype {
    GRAM_EOF = 0,
    ID = 300,
    STRING,
    BRACED_CODE,
    INT,
    PERCENT_DEFINE,
    PERCENT_DEFINES,
    PERCENT_VERBOSE,
    SEMICOLON,
    PERCENT_TOKEN,
    PERCENT_TYPE,
    PERCENT_EMPTY,
    COLON,
    PERCENT_EXPECT,
    PERCENT_PERCENT,
    PERCENT_INITIAL_ACTION,
    BRACKETED_ID,
    PIPE,
    EPILOGUE,
    TAG,
    PERCENT_CODE,
    ID_COLON,

    PERCENT_NONASSOC,
    PERCENT_FLAG,
    PERCENT_DEFAULT_PREC,
    PERCENT_DESTRUCTOR,
    PERCENT_DPREC,
    PERCENT_EXPECT_RR,
    EQUAL,
    TAG_ANY,
    CHAR,
    BRACED_PREDICATE,
    PERCENT_START,
    PERCENT_UNION,
    PERCENT_ERROR_VERBOSE,
    PERCENT_NAME_PREFIX,
    PERCENT_PURE_PARSER,
    PERCENT_RIGHT,
    PERCENT_PREC,
    PERCENT_FILE_PREFIX,
    PERCENT_YACC,
    PERCENT_GLR_PARSER,
    PERCENT_LANGUAGE,
    PERCENT_LEFT,
    PERCENT_PARAM,
    PERCENT_MERGE,
    PERCENT_NO_DEFAULT_PREC,
    PERCENT_NO_LINES,
    PERCENT_NONDETERMINISTIC_PARSER,
    PERCENT_NTERM,
    PERCENT_OUTPUT,
    PERCENT_PRECEDENCE,
    PRECENT_PRINTER,
    PERCENT_REQUIRE,
    PERCENT_SKELETON,
    PERCENT_TOKEN_TABLE,
    TAG_NONE,
    PROLOGUE,
    PERCENT_PRINTER,
};

union GRAM_STYPE {
    value_type value;
    uniqstr ID;
    char *STRING;
    symbol *symbol;
    named_ref *named_ref_opt;
    uniqstr tag_opt;
    char *BRACED_CODE;
    char *EPILOGUE;
    symbol *id_colon;
    uniqstr ID_COLON;
    int int_opt;
    int INT;
    symbol *string_as_id;
    symbol *string_as_id_opt;
    symbol *token_decl;
    symbol_list *token_decl_1;
    symbol_list *token_decls;
    symbol_list *symbol_decls;
    symbol_list *symbol_decl_1;
    uniqstr BRACKETED_ID;
    symbol *id;
    uniqstr TAG;

    uniqstr PERCENT_FLAG;
    uniqstr PERCENT_FILE_PREFIX;
    uniqstr PERCENT_NAME_PREFIX;
    uniqstr PERCENT_YACC;
    param_type PERCENT_PARAM;
    uniqstr PERCENT_PURE_PARSER;
    uniqstr PERCENT_ERROR_VERBOSE;
    unsigned char CHAR;
    char *BRACED_PREDICATE;
    char *PROLOGUE;
};
typedef union GRAM_STYPE GRAM_STYPE;

int gram_parse (void);

#endif

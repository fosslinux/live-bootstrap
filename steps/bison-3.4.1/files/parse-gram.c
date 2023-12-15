// SPDX-License-Identifier: GPL-3.0-or-later

/*
 * This file is based on parse-gram.y from GNU Bison 3.4.1. It
 * implements a subset of the grammar described by parse-gram.y, just
 * enough to provide a bootstrapping path for Bison.
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

/*
 * Since this code is intended to provide a bootstrapping path for GNU
 * Bison, it is important that it is as easy to understand as
 * possible. Fortunately Bison's grammar is rather simple. I believe
 * that if you know how Bison works, you should not find it hard to
 * follow the logic in this file and compare it with the original
 * grammar.
 *
 * Apart from the code literally copied from the grammar file
 * (prologues and epilogue), this file basically contains a few
 * wrappers over the lexer interface (the functions gram_*), basically
 * adding the feature to give back one lexeme and a lot of functions
 * parse_* or maybe_parse_*, parsing the corresponding grammar
 * rule. Functions maybe_parse_* are allowed to fail, and return
 * accordingly.
 *
 * Error checking is very simple and just made of assertions. This is
 * just boostrapping code, not meant for end users, so it should not
 * be a problem.
 */

/* %code top */
  /* On column 0 to please syntax-check.  */
#include <config.h>

#include <assert.h>

#define YYLTYPE GRAM_LTYPE

#include "parse-gram.h"

/* %code */
  #include "system.h"
  #include <errno.h>

  #include "c-ctype.h"
  #include "complain.h"
  #include "conflicts.h"
  #include "files.h"
  #include "getargs.h"
  #include "gram.h"
  #include "named-ref.h"
  #include "quotearg.h"
  #include "reader.h"
  #include "scan-code.h"
  #include "scan-gram.h"
  #include "vasnprintf.h"
  #include "xmemdup0.h"

  static int current_prec = 0;
  static location current_lhs_loc;
  static named_ref *current_lhs_named_ref;
  static symbol *current_lhs_symbol;
  static symbol_class current_class = unknown_sym;

  /** Set the new current left-hand side symbol, possibly common
   * to several right-hand side parts of rule.
   */
  static void current_lhs (symbol *sym, location loc, named_ref *ref);

  #define YYLLOC_DEFAULT(Current, Rhs, N)         \
    (Current) = lloc_default (Rhs, N)
  static YYLTYPE lloc_default (YYLTYPE const *, int);

  #define YY_LOCATION_PRINT(File, Loc)            \
    location_print (Loc, File)

  /* Strip initial '{' and final '}' (must be first and last characters).
     Return the result.  */
  static char *strip_braces (char *code);

  /* Convert CODE by calling code_props_plain_init if PLAIN, otherwise
     code_props_symbol_action_init.  Call
     gram_scanner_last_string_free to release the latest string from
     the scanner (should be CODE). */
  static char const *translate_code (char *code, location loc, bool plain);

  /* Convert CODE by calling code_props_plain_init after having
     stripped the first and last characters (expected to be '{', and
     '}').  Call gram_scanner_last_string_free to release the latest
     string from the scanner (should be CODE). */
  static char const *translate_code_braceless (char *code, location loc);

  /* Handle a %error-verbose directive.  */
  static void handle_error_verbose (location const *loc, char const *directive);

  /* Handle a %file-prefix directive.  */
  static void handle_file_prefix (location const *loc,
                                  location const *dir_loc,
                                  char const *directive, char const *value);

  /* Handle a %name-prefix directive.  */
  static void handle_name_prefix (location const *loc,
                                  char const *directive, char const *value);

  /* Handle a %pure-parser directive.  */
  static void handle_pure_parser (location const *loc, char const *directive);

  /* Handle a %require directive.  */
  static void handle_require (location const *loc, char const *version);

  /* Handle a %skeleton directive.  */
  static void handle_skeleton (location const *loc, char const *skel);

  /* Handle a %yacc directive.  */
  static void handle_yacc (location const *loc, char const *directive);

  static void gram_error (location const *, char const *);

  /* A string that describes a char (e.g., 'a' -> "'a'").  */
  static char const *char_name (char);

  #define YYTYPE_INT16 int_fast16_t
  #define YYTYPE_INT8 int_fast8_t
  #define YYTYPE_UINT16 uint_fast16_t
  #define YYTYPE_UINT8 uint_fast8_t

  /* Add style to semantic values in traces.  */
  static void tron (FILE *yyo);
  static void troff (FILE *yyo);

int gram_debug;

typedef struct {
    GRAM_STYPE s;
    GRAM_LTYPE l;
    int t;
} gram_lex_ctx;

static int gram_lex_wrap(gram_lex_ctx *ctx, GRAM_STYPE *sx, GRAM_LTYPE *lx) {
    int type;
    if (ctx->t == 0) {
        type = gram_lex(&ctx->s, &ctx->l);
    } else {
        type = ctx->t;
    }
    /*fprintf(stderr, "%s %d from %s:%d:%d:%d to %s:%d:%d:%d\n",
            ctx->t == 0 ? "Scanned" : "Rescanned", type,
            ctx->l.start.file, ctx->l.start.line, ctx->l.start.column, ctx->l.start.byte,
            ctx->l.end.file, ctx->l.end.line, ctx->l.end.column, ctx->l.end.byte);*/
    ctx->t = 0;
    if (sx) *sx = ctx->s;
    if (lx) *lx = ctx->l;
    return type;
}

static int gram_unlex_wrap(gram_lex_ctx *ctx, int t, GRAM_STYPE *s, GRAM_LTYPE *l) {
    assert(ctx->t == 0);
    ctx->t = t;
    ctx->s = *s;
    ctx->l = *l;
    /*fprintf(stderr, "Unscanned %d from %s:%d:%d:%d to %s:%d:%d:%d\n",
            t,
            ctx->l.start.file, ctx->l.start.line, ctx->l.start.column, ctx->l.start.byte,
            ctx->l.end.file, ctx->l.end.line, ctx->l.end.column, ctx->l.end.byte);*/
}

static void gram_get_last_loc(gram_lex_ctx *ctx, GRAM_LTYPE *l) {
    *l = ctx->l;
}

static void parse_value(gram_lex_ctx *ctx, GRAM_STYPE *sx, GRAM_LTYPE *lx) {
    GRAM_STYPE s[2];
    GRAM_LTYPE l[2];
    gram_get_last_loc(ctx, &l[0]);
    int t = gram_lex_wrap(ctx, &s[1], &l[1]);
    switch (t) {
    case ID:
        YYLLOC_DEFAULT(*lx, l, 1);
        sx->value.kind = muscle_keyword;
        sx->value.chars = s[1].ID;
        break;
    case STRING:
        YYLLOC_DEFAULT(*lx, l, 1);
        sx->value.kind = muscle_string;
        sx->value.chars = s[1].STRING;
        break;
    case BRACED_CODE:
        YYLLOC_DEFAULT(*lx, l, 1);
        sx->value.kind = muscle_code;
        sx->value.chars = strip_braces(s[1].BRACED_CODE);
        break;
    default:
        gram_unlex_wrap(ctx, t, &s[1], &l[1]);
        YYLLOC_DEFAULT(*lx, l, 0);
        sx->value.kind = muscle_keyword;
        sx->value.chars = "";
        break;
    }
}

static void parse_int_opt(gram_lex_ctx *ctx, GRAM_STYPE *sx, GRAM_LTYPE *lx) {
    GRAM_STYPE s[2];
    GRAM_LTYPE l[2];
    gram_get_last_loc(ctx, &l[0]);
    int t = gram_lex_wrap(ctx, &s[1], &l[1]);
    if (t == INT) {
        YYLLOC_DEFAULT(*lx, l, 1);
        sx->int_opt = s[1].INT;
    } else {
        gram_unlex_wrap(ctx, t, &s[1], &l[1]);
        YYLLOC_DEFAULT(*lx, l, 0);
        sx->int_opt = -1;
    }
}

static int maybe_parse_string_as_id(gram_lex_ctx *ctx, GRAM_STYPE *sx, GRAM_LTYPE *lx) {
    GRAM_STYPE s[2];
    GRAM_LTYPE l[2];
    int t = gram_lex_wrap(ctx, &s[1], &l[1]);
    if (t == STRING) {
        YYLLOC_DEFAULT(*lx, l, 1);
        sx->string_as_id = symbol_get(quotearg_style(c_quoting_style, s[1].STRING), l[1]);
        symbol_class_set(sx->string_as_id, token_sym, l[1], false);
        return 1;
    } else {
        gram_unlex_wrap(ctx, t, &s[1], &l[1]);
        return 0;
    }
}

static void parse_string_as_id(gram_lex_ctx *ctx, GRAM_STYPE *sx, GRAM_LTYPE *lx) {
    int x = maybe_parse_string_as_id(ctx, sx, lx);
    assert(x);
}

static void parse_string_as_id_opt(gram_lex_ctx *ctx, GRAM_STYPE *sx, GRAM_LTYPE *lx) {
    GRAM_STYPE s[2];
    GRAM_LTYPE l[2];
    gram_get_last_loc(ctx, &l[0]);
    int x = maybe_parse_string_as_id(ctx, &s[1], &l[1]);
    if (x) {
        YYLLOC_DEFAULT(*lx, l, 1);
        sx->string_as_id_opt = s[1].string_as_id;
    } else {
        YYLLOC_DEFAULT(*lx, l, 0);
        sx->string_as_id_opt = NULL;
    }
}

static int maybe_parse_id(gram_lex_ctx *ctx, GRAM_STYPE *sx, GRAM_LTYPE *lx) {
    GRAM_STYPE s[2];
    GRAM_LTYPE l[2];
    gram_get_last_loc(ctx, &l[0]);
    int t = gram_lex_wrap(ctx, &s[1], &l[1]);
    if (t == ID) {
        YYLLOC_DEFAULT(*lx, l, 1);
        sx->id = symbol_from_uniqstr(s[1].ID, l[1]);
        return 1;
    } else {
        gram_unlex_wrap(ctx, t, &s[1], &l[1]);
        return 0;
    }
}

static void parse_id(gram_lex_ctx *ctx, GRAM_STYPE *sx, GRAM_LTYPE *lx) {
    int x = maybe_parse_id(ctx, sx, lx);
    assert(x);
}

static void parse_token_decl(gram_lex_ctx *ctx, GRAM_STYPE *sx, GRAM_LTYPE *lx) {
    GRAM_STYPE s[4];
    GRAM_LTYPE l[4];
    parse_id(ctx, &s[1], &l[1]);
    parse_int_opt(ctx, &s[2], &l[2]);
    parse_string_as_id_opt(ctx, &s[3], &l[3]);
    YYLLOC_DEFAULT(*lx, l, 3);
    sx->token_decl = s[1].id;
    symbol_class_set(s[1].id, current_class, l[1], true);
    if (0 <= s[2].int_opt)
        symbol_user_token_number_set(s[1].id, s[2].int_opt, l[2]);
    if (s[3].string_as_id_opt)
        symbol_make_alias(s[1].id, s[3].string_as_id_opt, l[3]);
}

static int maybe_parse_token_decl_1(gram_lex_ctx *ctx, GRAM_STYPE *sx, GRAM_LTYPE *lx) {
    GRAM_STYPE s[3];
    GRAM_LTYPE l[3];
    int t = gram_lex_wrap(ctx, &s[1], &l[1]);
    gram_unlex_wrap(ctx, t, &s[1], &l[1]);
    if (t != ID) {
        return 0;
    }
    parse_token_decl(ctx, &s[1], &l[1]);
    YYLLOC_DEFAULT(*lx, l, 1);
    sx->token_decl_1 = symbol_list_sym_new(s[1].token_decl, l[1]);
    while (1) {
        int t = gram_lex_wrap(ctx, &s[1], &l[1]);
        gram_unlex_wrap(ctx, t, &s[1], &l[1]);
        if (t != ID) {
            return 1;
        }
        s[1] = *sx;
        l[1] = *lx;
        parse_token_decl(ctx, &s[2], &l[2]);
        YYLLOC_DEFAULT(*lx, l, 2);
        sx->token_decl_1 = symbol_list_append(s[1].token_decl_1, symbol_list_sym_new(s[2].token_decl, l[2]));
    }
}

static void parse_token_decl_1(gram_lex_ctx *ctx, GRAM_STYPE *sx, GRAM_LTYPE *lx) {
    int x = maybe_parse_token_decl_1(ctx, sx, lx);
    assert(x);
}

static void parse_token_decls(gram_lex_ctx *ctx, GRAM_STYPE *sx, GRAM_LTYPE *lx) {
    GRAM_STYPE s[4];
    GRAM_LTYPE l[4];
    int begin_with_tokens = maybe_parse_token_decl_1(ctx, &s[1], &l[1]);
    if (begin_with_tokens) {
        YYLLOC_DEFAULT(*lx, l, 1);
        sx->token_decls = s[1].token_decl_1;
    } else {
        int t = gram_lex_wrap(ctx, &s[1], &l[1]);
        assert(t == TAG);
        parse_token_decl_1(ctx, &s[2], &l[2]);
        YYLLOC_DEFAULT(*lx, l, 2);
        sx->token_decls = symbol_list_type_set(s[2].token_decl_1, s[1].TAG, l[1]);
    }
    while (1) {
        s[1] = *sx;
        l[1] = *lx;
        int t = gram_lex_wrap(ctx, &s[2], &l[2]);
        if (t != TAG) {
            gram_unlex_wrap(ctx, t, &s[2], &l[2]);
            return;
        }
        parse_token_decl_1(ctx, &s[3], &l[3]);
        YYLLOC_DEFAULT(*lx, l, 3);
        sx->token_decls = symbol_list_append(s[1].token_decls, symbol_list_type_set(s[3].token_decl_1, s[2].TAG, l[2]));
    }
}

static int maybe_parse_symbol(gram_lex_ctx *ctx, GRAM_STYPE *sx, GRAM_LTYPE *lx) {
    GRAM_STYPE s[2];
    GRAM_LTYPE l[2];
    int x = maybe_parse_string_as_id(ctx, &s[1], &l[1]);
    if (x) {
        YYLLOC_DEFAULT(*lx, l, 1);
        sx->symbol = s[1].string_as_id;
        return 1;
    }
    x = maybe_parse_id(ctx, &s[1], &l[1]);
    if (x) {
        YYLLOC_DEFAULT(*lx, l, 1);
        sx->symbol = s[1].id;
        return 1;
    }
    return 0;
}

static void parse_symbol(gram_lex_ctx *ctx, GRAM_STYPE *sx, GRAM_LTYPE *lx) {
    int x = maybe_parse_symbol(ctx, sx, lx);
    assert(x);
}

static int maybe_parse_symbol_decl_1(gram_lex_ctx *ctx, GRAM_STYPE *sx, GRAM_LTYPE *lx) {
    GRAM_STYPE s[3];
    GRAM_LTYPE l[3];
    int x = maybe_parse_symbol(ctx, &s[1], &l[1]);
    if (!x) {
        return 0;
    }
    YYLLOC_DEFAULT(*lx, l, 1);
    sx->symbol_decl_1 = symbol_list_sym_new(s[1].symbol, l[1]);
    while (1) {
        s[1] = *sx;
        l[1] = *lx;
        x = maybe_parse_symbol(ctx, &s[2], &l[2]);
        if (!x) {
            return 1;
        }
        YYLLOC_DEFAULT(*lx, l, 2);
        sx->symbol_decl_1 = symbol_list_append(s[1].symbol_decl_1, symbol_list_sym_new(s[2].symbol, l[2]));
    }
}

static void parse_symbol_decl_1(gram_lex_ctx *ctx, GRAM_STYPE *sx, GRAM_LTYPE *lx) {
    int x = maybe_parse_symbol_decl_1(ctx, sx, lx);
    assert(x);
}

static void parse_symbol_decls(gram_lex_ctx *ctx, GRAM_STYPE *sx, GRAM_LTYPE *lx) {
    GRAM_STYPE s[4];
    GRAM_LTYPE l[4];
    int begin_with_tokens = maybe_parse_symbol_decl_1(ctx, &s[1], &l[1]);
    if (begin_with_tokens) {
        YYLLOC_DEFAULT(*lx, l, 1);
        sx->symbol_decls = s[1].symbol_decl_1;
    } else {
        int t = gram_lex_wrap(ctx, &s[1], &l[1]);
        assert(t == TAG);
        parse_symbol_decl_1(ctx, &s[2], &l[2]);
        YYLLOC_DEFAULT(*lx, l, 2);
        sx->symbol_decls = symbol_list_type_set(s[2].symbol_decl_1, s[1].TAG, l[1]);
    }
    while (1) {
        s[1] = *sx;
        l[1] = *lx;
        int t = gram_lex_wrap(ctx, &s[2], &l[2]);
        if (t != TAG) {
            gram_unlex_wrap(ctx, t, &s[2], &l[2]);
            return;
        }
        parse_symbol_decl_1(ctx, &s[3], &l[3]);
        YYLLOC_DEFAULT(*lx, l, 3);
        sx->symbol_decls = symbol_list_append(s[1].symbol_decls, symbol_list_type_set(s[3].symbol_decl_1, s[2].TAG, l[2]));
    }
}

static int maybe_parse_declaration(gram_lex_ctx *ctx, int prologue) {
    GRAM_STYPE s[4];
    GRAM_LTYPE l[4];
    int t[4];
    t[1] = gram_lex_wrap(ctx, &s[1], &l[1]);
    if (prologue) {
        switch (t[1]) {
            /* Prologue declarations */
        case PERCENT_DEFINE:
            t[2] = gram_lex_wrap(ctx, &s[2], &l[2]);
            assert(t[2] == ID);
            parse_value(ctx, &s[3], &l[3]);
            YYLLOC_DEFAULT(l[0], l, 3);
            muscle_percent_define_insert(s[2].ID, l[0], s[3].value.kind, s[3].value.chars,
                                         MUSCLE_PERCENT_DEFINE_GRAMMAR_FILE);
            return 1;
        case PERCENT_DEFINES:
            defines_flag = true;
            return 1;
        case PERCENT_EXPECT:
            t[2] = gram_lex_wrap(ctx, &s[2], &l[2]);
            assert(t[2] == INT);
            expected_sr_conflicts = s[2].INT;
            return 1;
        case PERCENT_INITIAL_ACTION:
            t[2] = gram_lex_wrap(ctx, &s[2], &l[2]);
            assert(t[2] == BRACED_CODE);
            muscle_code_grow("initial_action", translate_code(s[2].BRACED_CODE, l[2], false), l[2]);
            code_scanner_last_string_free();
            return 1;
        case PERCENT_VERBOSE:
            report_flag |= report_states;
            return 1;
        case SEMICOLON:
            return 1;
        }
    }

    switch (t[1]) {
        /* Grammar declarations */
    case PERCENT_CODE:
        t[2] = gram_lex_wrap(ctx, &s[2], &l[2]);
        if (t[2] == ID) {
            t[3] = gram_lex_wrap(ctx, &s[3], &l[3]);
            assert(t[3] == BRACED_CODE);
            muscle_percent_code_grow(s[2].ID, l[2], translate_code_braceless(s[3].BRACED_CODE, l[3]), l[3]);
            code_scanner_last_string_free();
        } else {
            assert(t[2] == BRACED_CODE);
            muscle_code_grow("percent_code()", translate_code_braceless(s[2].BRACED_CODE, l[2]), l[2]);
            code_scanner_last_string_free();
        }
        return 1;

        /* Symbol declarations */
    case PERCENT_TOKEN:
        current_class = token_sym;
        parse_token_decls(ctx, &s[2], &l[2]);
        current_class = unknown_sym;
        symbol_list_free(s[2].token_decls);
        return 1;
    case PERCENT_TYPE:
        parse_symbol_decls(ctx, &s[2], &l[2]);
        symbol_list_free(s[2].symbol_decls);
        return 1;
    case PERCENT_PERCENT:
        return 2;
    default:
        gram_unlex_wrap(ctx, t[1], &s[1], &l[1]);
        return 0;
    }
}

static void parse_prologue_declarations(gram_lex_ctx *ctx) {
    while (1) {
        int x = maybe_parse_declaration(ctx, 1);
        assert(x);
        if (x == 2) {
            return;
        }
    }
}

static void parse_id_colon(gram_lex_ctx *ctx, GRAM_STYPE *sx, GRAM_LTYPE *lx) {
    GRAM_STYPE s[2];
    GRAM_LTYPE l[2];
    int t = gram_lex_wrap(ctx, &s[1], &l[1]);
    assert(t == ID_COLON);
    YYLLOC_DEFAULT(*lx, l, 1);
    sx->id_colon = symbol_from_uniqstr(s[1].ID_COLON, l[1]);
}

static void parse_named_ref_opt(gram_lex_ctx *ctx, GRAM_STYPE *sx, GRAM_LTYPE *lx) {
    GRAM_STYPE s[2];
    GRAM_LTYPE l[2];
    gram_get_last_loc(ctx, &l[0]);
    int t = gram_lex_wrap(ctx, &s[1], &l[1]);
    if (t == BRACKETED_ID) {
        YYLLOC_DEFAULT(*lx, l, 1);
        sx->named_ref_opt = named_ref_new(s[1].BRACKETED_ID, l[1]);
    } else {
        gram_unlex_wrap(ctx, t, &s[1], &l[1]);
        YYLLOC_DEFAULT(*lx, l, 0);
        sx->named_ref_opt = NULL;
    }
}

static void parse_rhs(gram_lex_ctx *ctx, GRAM_STYPE *sx, GRAM_LTYPE *lx) {
    GRAM_STYPE s[5];
    GRAM_LTYPE l[5];
    gram_get_last_loc(ctx, &l[0]);
    YYLLOC_DEFAULT(*lx, l, 0);
    grammar_current_rule_begin(current_lhs_symbol, current_lhs_loc, current_lhs_named_ref);
    while (1) {
        s[1] = *sx;
        l[1] = *lx;
        int x = maybe_parse_symbol(ctx, &s[2], &l[2]);
        if (x) {
            parse_named_ref_opt(ctx, &s[3], &l[3]);
            YYLLOC_DEFAULT(*lx, l, 3);
            grammar_current_rule_symbol_append(s[2].symbol, l[2], s[3].named_ref_opt);
        } else {
            int t = gram_lex_wrap(ctx, &s[2], &l[2]);
            switch (t) {
            case BRACED_CODE:
                /* Probably good */
                s[3] = s[2];
                l[3] = l[2];
                s[2].tag_opt = NULL;
                l[2] = l[1];
                parse_named_ref_opt(ctx, &s[4], &l[4]);
                YYLLOC_DEFAULT(*lx, l, 4);
                grammar_current_rule_action_append(s[3].BRACED_CODE, l[3], s[4].named_ref_opt, s[2].tag_opt);
                break;
            case PERCENT_EMPTY:
                YYLLOC_DEFAULT(*lx, l, 2);
                grammar_current_rule_empty_set(l[2]);
                break;
            default:
                gram_unlex_wrap(ctx, t, &s[2], &l[2]);
                return;
            }
        }
    }
}

static void parse_rhses_1(gram_lex_ctx *ctx, GRAM_STYPE *sx, GRAM_LTYPE *lx) {
    GRAM_STYPE s[4];
    GRAM_LTYPE l[4];
    parse_rhs(ctx, &s[1], &l[1]);
    YYLLOC_DEFAULT(*lx, l, 1);
    grammar_current_rule_end(l[1]);
    while (1) {
        s[1] = *sx;
        l[1] = *lx;
        int t = gram_lex_wrap(ctx, &s[2], &l[2]);
        switch (t) {
        case PIPE:
            parse_rhs(ctx, &s[3], &l[3]);
            YYLLOC_DEFAULT(*lx, l, 3);
            grammar_current_rule_end(l[3]);
            break;
        case SEMICOLON:
            YYLLOC_DEFAULT(*lx, l, 2);
            break;
        default:
            gram_unlex_wrap(ctx, t, &s[2], &l[2]);
            return;
        }
    }
}

static void parse_rules(gram_lex_ctx *ctx) {
    GRAM_STYPE s[6];
    GRAM_LTYPE l[6];
    parse_id_colon(ctx, &s[1], &l[1]);
    parse_named_ref_opt(ctx, &s[2], &l[2]);
    gram_get_last_loc(ctx, &l[3]);
    current_lhs(s[1].id_colon, l[1], s[2].named_ref_opt);
    int t = gram_lex_wrap(ctx, &s[4], &l[4]);
    assert(t == COLON);
    parse_rhses_1(ctx, &s[5], &l[5]);
    current_lhs(0, l[1], 0);
}

static void parse_grammar(gram_lex_ctx *ctx) {
    while (1) {
        int x = maybe_parse_declaration(ctx, 0);
        if (x == 1) {
            int t = gram_lex_wrap(ctx, NULL, NULL);
            assert(t == SEMICOLON);
        } if (x == 2) {
            return;
        } else if (x == 0) {
            parse_rules(ctx);
        }
    }
}

static void parse_epilogue(gram_lex_ctx *ctx) {
    GRAM_STYPE s;
    GRAM_LTYPE l;
    int t = gram_lex_wrap(ctx, &s, &l);
    assert(t == EPILOGUE);
    muscle_code_grow("epilogue", translate_code(s.EPILOGUE, l, true), l);
    code_scanner_last_string_free();
}

int gram_parse(void) {
    gram_lex_ctx ctx = {};
    boundary_set(&ctx.l.start, current_file, 1, 1, 1);
    boundary_set(&ctx.l.end, current_file, 1, 1, 1);
    parse_prologue_declarations(&ctx);
    parse_grammar(&ctx);
    parse_epilogue(&ctx);
    assert(gram_lex_wrap(&ctx, NULL, NULL) == 0);
    return 0;
}

// Epilogue

/* Return the location of the left-hand side of a rule whose
   right-hand side is RHS[1] ... RHS[N].  Ignore empty nonterminals in
   the right-hand side, and return an empty location equal to the end
   boundary of RHS[0] if the right-hand side is empty.  */

static YYLTYPE
lloc_default (YYLTYPE const *rhs, int n)
{
  YYLTYPE loc;

  /* SGI MIPSpro 7.4.1m miscompiles "loc.start = loc.end = rhs[n].end;".
     The bug is fixed in 7.4.2m, but play it safe for now.  */
  loc.start = rhs[n].end;
  loc.end = rhs[n].end;

  /* Ignore empty nonterminals the start of the right-hand side.
     Do not bother to ignore them at the end of the right-hand side,
     since empty nonterminals have the same end as their predecessors.  */
  for (int i = 1; i <= n; i++)
    if (! equal_boundaries (rhs[i].start, rhs[i].end))
      {
        loc.start = rhs[i].start;
        break;
      }

  return loc;
}

static
char *strip_braces (char *code)
{
  code[strlen (code) - 1] = 0;
  return code + 1;
}

static
char const *
translate_code (char *code, location loc, bool plain)
{
  code_props plain_code;
  if (plain)
    code_props_plain_init (&plain_code, code, loc);
  else
    code_props_symbol_action_init (&plain_code, code, loc);
  code_props_translate_code (&plain_code);
  gram_scanner_last_string_free ();
  return plain_code.code;
}

static
char const *
translate_code_braceless (char *code, location loc)
{
  return translate_code (strip_braces (code), loc, true);
}

static void
add_param (param_type type, char *decl, location loc)
{
  static char const alphanum[26 + 26 + 1 + 10 + 1] =
    "abcdefghijklmnopqrstuvwxyz"
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "_"
    "0123456789";

  char const *name_start = NULL;
  {
    char *p;
    /* Stop on last actual character.  */
    for (p = decl; p[1]; p++)
      if ((p == decl
           || ! memchr (alphanum, p[-1], sizeof alphanum - 1))
          && memchr (alphanum, p[0], sizeof alphanum - 10 - 1))
        name_start = p;

    /* Strip the surrounding '{' and '}', and any blanks just inside
       the braces.  */
    --p;
    while (c_isspace ((unsigned char) *p))
      --p;
    p[1] = '\0';
    ++decl;
    while (c_isspace ((unsigned char) *decl))
      ++decl;
  }

  if (! name_start)
    complain (&loc, complaint, _("missing identifier in parameter declaration"));
  else
    {
      char *name = xmemdup0 (name_start, strspn (name_start, alphanum));
      if (type & param_lex)
        muscle_pair_list_grow ("lex_param", decl, name);
      if (type & param_parse)
        muscle_pair_list_grow ("parse_param", decl, name);
      free (name);
    }

  gram_scanner_last_string_free ();
}


static void
handle_error_verbose (location const *loc, char const *directive)
{
  bison_directive (loc, directive);
  muscle_percent_define_insert (directive, *loc, muscle_keyword, "",
                                MUSCLE_PERCENT_DEFINE_GRAMMAR_FILE);
}


static void
handle_file_prefix (location const *loc,
                    location const *dir_loc,
                    char const *directive, char const *value)
{
  bison_directive (loc, directive);
  bool warned = false;

  if (location_empty (spec_file_prefix_loc))
    {
      spec_file_prefix_loc = *loc;
      spec_file_prefix = value;
    }
  else
    {
      duplicate_directive (directive, spec_file_prefix_loc, *loc);
      warned = true;
    }

  if (!warned
      && STRNEQ (directive, "%file-prefix"))
    deprecated_directive (dir_loc, directive, "%file-prefix");
}


static void
handle_name_prefix (location const *loc,
                    char const *directive, char const *value)
{
  bison_directive (loc, directive);

  char buf1[1024];
  size_t len1 = sizeof (buf1);
  char *old = asnprintf (buf1, &len1, "%s\"%s\"", directive, value);
  if (!old)
    xalloc_die ();

  if (location_empty (spec_name_prefix_loc))
    {
      spec_name_prefix = value;
      spec_name_prefix_loc = *loc;

      char buf2[1024];
      size_t len2 = sizeof (buf2);
      char *new = asnprintf (buf2, &len2, "%%define api.prefix {%s}", value);
      if (!new)
        xalloc_die ();
      deprecated_directive (loc, old, new);
      if (new != buf2)
        free (new);
    }
  else
    duplicate_directive (old, spec_file_prefix_loc, *loc);

  if (old != buf1)
    free (old);
}


static void
handle_pure_parser (location const *loc, char const *directive)
{
  bison_directive (loc, directive);
  deprecated_directive (loc, directive, "%define api.pure");
  muscle_percent_define_insert ("api.pure", *loc, muscle_keyword, "",
                                MUSCLE_PERCENT_DEFINE_GRAMMAR_FILE);
}


static void
handle_require (location const *loc, char const *version)
{
  /* Changes of behavior are only on minor version changes, so "3.0.5"
     is the same as "3.0". */
  errno = 0;
  char* cp = NULL;
  unsigned long major = strtoul (version, &cp, 10);
  if (errno || *cp != '.')
    {
      complain (loc, complaint, _("invalid version requirement: %s"),
                version);
      return;
    }
  ++cp;
  unsigned long minor = strtoul (cp, NULL, 10);
  if (errno)
    {
      complain (loc, complaint, _("invalid version requirement: %s"),
                version);
      return;
    }
  required_version = major * 100 + minor;
  /* Pretend to be at least 3.4, to check features published in 3.4
     while developping it.  */
  const char* api_version = "3.4";
  const char* package_version =
    strverscmp (api_version, PACKAGE_VERSION) > 0
    ? api_version : PACKAGE_VERSION;
  if (strverscmp (version, package_version) > 0)
    {
      complain (loc, complaint, _("require bison %s, but have %s"),
                version, package_version);
      exit (EX_MISMATCH);
    }
}

static void
handle_skeleton (location const *loc, char const *skel)
{
  char const *skeleton_user = skel;
  if (strchr (skeleton_user, '/'))
    {
      size_t dir_length = strlen (current_file);
      while (dir_length && current_file[dir_length - 1] != '/')
        --dir_length;
      while (dir_length && current_file[dir_length - 1] == '/')
        --dir_length;
      char *skeleton_build =
        xmalloc (dir_length + 1 + strlen (skeleton_user) + 1);
      if (dir_length > 0)
        {
          memcpy (skeleton_build, current_file, dir_length);
          skeleton_build[dir_length++] = '/';
        }
      strcpy (skeleton_build + dir_length, skeleton_user);
      skeleton_user = uniqstr_new (skeleton_build);
      free (skeleton_build);
    }
  skeleton_arg (skeleton_user, grammar_prio, *loc);
}


static void
handle_yacc (location const *loc, char const *directive)
{
  bison_directive (loc, directive);
  bool warned = false;

  if (location_empty (yacc_loc))
    yacc_loc = *loc;
  else
    {
      duplicate_directive (directive, yacc_loc, *loc);
      warned = true;
    }

  if (!warned
      && STRNEQ (directive, "%fixed-output-files")
      && STRNEQ (directive, "%yacc"))
    deprecated_directive (loc, directive, "%fixed-output-files");
}


static void
gram_error (location const *loc, char const *msg)
{
  complain (loc, complaint, "%s", msg);
}

static char const *
char_name (char c)
{
  if (c == '\'')
    return "'\\''";
  else
    {
      char buf[4];
      buf[0] = '\''; buf[1] = c; buf[2] = '\''; buf[3] = '\0';
      return quotearg_style (escape_quoting_style, buf);
    }
}

static
void
current_lhs (symbol *sym, location loc, named_ref *ref)
{
  current_lhs_symbol = sym;
  current_lhs_loc = loc;
  if (sym)
    symbol_location_as_lhs_set (sym, loc);
  /* In order to simplify memory management, named references for lhs
     are always assigned by deep copy into the current symbol_list
     node.  This is because a single named-ref in the grammar may
     result in several uses when the user factors lhs between several
     rules using "|".  Therefore free the parser's original copy.  */
  free (current_lhs_named_ref);
  current_lhs_named_ref = ref;
}

static void tron (FILE *yyo)
{
  begin_use_class ("value", yyo);
}

static void troff (FILE *yyo)
{
  end_use_class ("value", yyo);
}

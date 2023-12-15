/* SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu> */
/* SPDX-FileCopyrightText: 2019-2020 Giovanni Mascellani <gio@debian.org> */

/* SPDX-License-Identifier: BSD-2-Clause */

/* scan.l - scanner for flex input -*-C-*- */

%{
/*  Copyright (c) 1990 The Regents of the University of California. */
/*  All rights reserved. */

/*  This code is derived from software contributed to Berkeley by */
/*  Vern Paxson. */

/*  The United States Government has rights in this work pursuant */
/*  to contract no. DE-AC03-76SF00098 between the United States */
/*  Department of Energy and the University of California. */

/*  This file is part of flex. */

/*  Redistribution and use in source and binary forms, with or without */
/*  modification, are permitted provided that the following conditions */
/*  are met: */

/*  1. Redistributions of source code must retain the above copyright */
/*     notice, this list of conditions and the following disclaimer. */
/*  2. Redistributions in binary form must reproduce the above copyright */
/*     notice, this list of conditions and the following disclaimer in the */
/*     documentation and/or other materials provided with the distribution. */

/*  Neither the name of the University nor the names of its contributors */
/*  may be used to endorse or promote products derived from this software */
/*  without specific prior written permission. */

/*  THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR */
/*  IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED */
/*  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR */
/*  PURPOSE. */

#define yyin_defined
#include "flexdef.h"
#include "parse.h"

#define ACTION_ECHO add_action( yytext )
#define ACTION_IFDEF(def, should_define) \
	{ \
	if ( should_define ) \
		action_define( def, 1 ); \
	}

#define MARK_END_OF_PROLOG mark_prolog();

#define YY_DECL \
	int flexscan()

#define RETURNCHAR \
	yylval = (unsigned char) yytext[0]; \
	return CHAR;

#define RETURNNAME \
	strcpy( nmstr, yytext ); \
	return NAME;

#define PUT_BACK_STRING(str, start) \
	for ( i = strlen( str ) - 1; i >= start; --i ) \
		unput((str)[i])

#define CHECK_REJECT(str) \
	if ( all_upper( str ) ) \
		reject = true;

#define CHECK_YYMORE(str) \
	if ( all_lower( str ) ) \
		yymore_used = true;

#define YY_USER_INIT \
	if ( getenv("POSIXLY_CORRECT") ) \
		posix_compat = true;

#ifndef yy_set_bol
#define yy_set_bol(x) (x ? NLSTATE : 0)
#endif

#define YY_NULL 0
#define yyterminate() return YY_NULL

%}

%e 5000
%p 10000
%a 100000
%n 5000

%x SECT2 SECT2PROLOG SECT3 CODEBLOCK PICKUPDEF SC CARETISBOL NUM QUOTE
%x FIRSTCCL CCL ACTION RECOVER COMMENT ACTION_STRING
%x OPTION LINEDIR

WS		[ \t]+
OPTWS		[ \t]*
NOT_WS		[^ \t\r\n]

NL		\r?\n

NAME		([a-zA-Z_][a-zA-Z0-9_-]*)
NOT_NAME	[^a-zA-Z_*\n]+

SCNAME		{NAME}

ESCSEQ		(\\([^\n]|[0-7]{1,3}|x[0-9a-fA-F]{1,2}))

FIRST_CCL_CHAR	([^\\\n]|{ESCSEQ})
CCL_CHAR	([^\\\n\]]|{ESCSEQ})
CCL_EXPR	("[:"[a-zA-Z]+":]")

LEXOPT		[aceknopr]

%%
	static int bracelevel, didadef, indented_code;
	static int doing_rule_action = false;
	static int option_sense;

	int doing_codeblock = false;
	int i;
	Char nmdef[MAXLINE];


<INITIAL>^{WS}		{ indented_code = true; BEGIN(CODEBLOCK); }
<INITIAL>^"/*"		{ ACTION_ECHO; BEGIN( COMMENT ); }
<INITIAL>^#{OPTWS}line{WS}	{ BEGIN( LINEDIR ); }
<INITIAL>^"%s"{NAME}?	{ return SCDECL; }
<INITIAL>^"%x"{NAME}?	{ return XSCDECL; }
<INITIAL>^"%{".*{NL}	{
			++linenum;
			line_directive_out( (FILE *) 0, 1 );
			indented_code = false;
			BEGIN(CODEBLOCK);
			}

<INITIAL>{WS}		{ /* discard */ }

<INITIAL>^"%%".*		{
			sectnum = 2;
			bracelevel = 0;
			mark_defs1();
			line_directive_out( (FILE *) 0, 1 );
			BEGIN(SECT2PROLOG);
			return SECTEND;
			}

<INITIAL>^"%pointer".*{NL}	{ yytext_is_array = false; ++linenum; }
<INITIAL>^"%array".*{NL}		{ yytext_is_array = true; ++linenum; }

<INITIAL>^"%option"	{ BEGIN(OPTION); return OPTION_OP; }

<INITIAL>^"%"{LEXOPT}{OPTWS}[0-9]*{OPTWS}{NL}	{ ++linenum; /* ignore */ }
<INITIAL>^"%"{LEXOPT}{WS}.*{NL}	{ ++linenum;	/* ignore */ }

<INITIAL>^"%"[^sxaceknopr{}].*	{ synerr( _( "unrecognized '%' directive" ) ); }

<INITIAL>^{NAME}		{
			strcpy( nmstr, yytext );
			didadef = false;
			BEGIN(PICKUPDEF);
			}

<INITIAL>{SCNAME}	{ RETURNNAME; }
<INITIAL>^{OPTWS}{NL}	{ ++linenum; /* allows blank lines in section 1 */ }
<INITIAL>{OPTWS}{NL}	{ ACTION_ECHO; ++linenum; /* maybe end of comment line */ }


<COMMENT>"*/"		{ ACTION_ECHO; BEGIN(INITIAL); }
<COMMENT>"*"		{ ACTION_ECHO; }
<COMMENT>[^*\n]+		{ ACTION_ECHO; }
<COMMENT>[^*\n]*{NL}	{ ++linenum; ACTION_ECHO; }

<LINEDIR>\n		{ BEGIN(INITIAL); }
<LINEDIR>[0-9]+	{ linenum = myctoi( yytext ); }

<LINEDIR>\"[^"\n]*\"	{
			flex_free( (void *) infilename );
			infilename = copy_string( yytext + 1 );
			infilename[strlen( infilename ) - 1] = '\0';
			}

<CODEBLOCK>^"%}".*{NL}	{ ++linenum; BEGIN(INITIAL); }

<CODEBLOCK>{NAME}|{NOT_NAME}|.	{ ACTION_ECHO; }

<CODEBLOCK>{NL}		{
			++linenum;
			ACTION_ECHO;
			if ( indented_code )
				BEGIN(INITIAL);
			}


<PICKUPDEF>{WS}		{ /* separates name and definition */ }

<PICKUPDEF>{NOT_WS}[^\r\n]*	{
			strcpy( (char *) nmdef, yytext );

			/* Skip trailing whitespace. */
			for ( i = strlen( (char *) nmdef ) - 1;
			      i >= 0 && (nmdef[i] == ' ' || nmdef[i] == '\t');
			      --i )
				;

			nmdef[i + 1] = '\0';

			ndinstal( nmstr, nmdef );
			didadef = true;
			}

<PICKUPDEF>{NL}		{
			if ( ! didadef )
				synerr( _( "incomplete name definition" ) );
			BEGIN(INITIAL);
			++linenum;
			}


<OPTION>{NL}		{ ++linenum; BEGIN(INITIAL); }
<OPTION>{WS}		{ option_sense = true; }

<OPTION>"="		{ return '='; }

<OPTION>no		{ option_sense = ! option_sense; }

<OPTION>7bit		{ csize = option_sense ? 128 : 256; }
<OPTION>8bit		{ csize = option_sense ? 256 : 128; }

<OPTION>align		{ long_align = option_sense; }
<OPTION>always-interactive	{
			action_define( "YY_ALWAYS_INTERACTIVE", option_sense );
			}
<OPTION>array		{ yytext_is_array = option_sense; }
<OPTION>backup		{ backing_up_report = option_sense; }
<OPTION>batch		{ interactive = ! option_sense; }
<OPTION>"c++"		{ C_plus_plus = option_sense; }
<OPTION>caseful|case-sensitive		{ caseins = ! option_sense; }
<OPTION>caseless|case-insensitive	{ caseins = option_sense; }
<OPTION>debug		{ ddebug = option_sense; }
<OPTION>default		{ spprdflt = ! option_sense; }
<OPTION>ecs		{ useecs = option_sense; }
<OPTION>fast		{
			useecs = usemecs = false;
			use_read = fullspd = true;
			}
<OPTION>full		{
			useecs = usemecs = false;
			use_read = fulltbl = true;
			}
<OPTION>input		{ ACTION_IFDEF("YY_NO_INPUT", ! option_sense); }
<OPTION>interactive	{ interactive = option_sense; }
<OPTION>lex-compat	{ lex_compat = option_sense; }
<OPTION>posix-compat	{ posix_compat = option_sense; }
<OPTION>main		{
			action_define( "YY_MAIN", option_sense );
            /* Override yywrap */
            if( option_sense == true )
                do_yywrap = false;
			}
<OPTION>meta-ecs	{ usemecs = option_sense; }
<OPTION>never-interactive	{
			action_define( "YY_NEVER_INTERACTIVE", option_sense );
			}
<OPTION>perf-report	{ performance_report += option_sense ? 1 : -1; }
<OPTION>pointer		{ yytext_is_array = ! option_sense; }
<OPTION>read		{ use_read = option_sense; }
<OPTION>reentrant   { reentrant = option_sense; }
<OPTION>reentrant-bison {
                /* reentrant-bison implies reentrant. */
                if ((reentrant_bison_pure = option_sense) != 0)
                    reentrant = 1;
            }    
<OPTION>reject		{ reject_really_used = option_sense; }
<OPTION>stack		{ action_define( "YY_STACK_USED", option_sense ); }
<OPTION>stdinit		{ do_stdinit = option_sense; }
<OPTION>stdout		{ use_stdout = option_sense; }
<OPTION>unistd      { ACTION_IFDEF("YY_NO_UNISTD_H", ! option_sense); }
<OPTION>unput		{ ACTION_IFDEF("YY_NO_UNPUT", ! option_sense); }
<OPTION>verbose		{ printstats = option_sense; }
<OPTION>warn		{ nowarn = ! option_sense; }
<OPTION>yylineno	{ do_yylineno = option_sense; ACTION_IFDEF("YY_USE_LINENO", option_sense); }
<OPTION>yymore		{ yymore_really_used = option_sense; }
<OPTION>yywrap      { do_yywrap = option_sense; }

<OPTION>yy_push_state	{ ACTION_IFDEF("YY_NO_PUSH_STATE", ! option_sense); }
<OPTION>yy_pop_state	{ ACTION_IFDEF("YY_NO_POP_STATE", ! option_sense); }
<OPTION>yy_top_state	{ ACTION_IFDEF("YY_NO_TOP_STATE", ! option_sense); }

<OPTION>yy_scan_buffer	{ ACTION_IFDEF("YY_NO_SCAN_BUFFER", ! option_sense); }
<OPTION>yy_scan_bytes	{ ACTION_IFDEF("YY_NO_SCAN_BYTES", ! option_sense); }
<OPTION>yy_scan_string	{ ACTION_IFDEF("YY_NO_SCAN_STRING", ! option_sense); }

<OPTION>yyalloc         { ACTION_IFDEF("YY_NO_FLEX_ALLOC", ! option_sense); }
<OPTION>yyrealloc       { ACTION_IFDEF("YY_NO_FLEX_REALLOC", ! option_sense); }
<OPTION>yyfree          { ACTION_IFDEF("YY_NO_FLEX_FREE", ! option_sense); }

<OPTION>yyget_debug     { ACTION_IFDEF("YY_NO_GET_DEBUG", ! option_sense); }
<OPTION>yyset_debug     { ACTION_IFDEF("YY_NO_SET_DEBUG", ! option_sense); }
<OPTION>yyget_extra     { ACTION_IFDEF("YY_NO_GET_EXTRA", ! option_sense); }
<OPTION>yyset_extra     { ACTION_IFDEF("YY_NO_SET_EXTRA", ! option_sense); }
<OPTION>yyget_leng      { ACTION_IFDEF("YY_NO_GET_LENG", ! option_sense); }
<OPTION>yyget_text      { ACTION_IFDEF("YY_NO_GET_TEXT", ! option_sense); }
<OPTION>yyget_lineno    { ACTION_IFDEF("YY_NO_GET_LINENO", ! option_sense); }
<OPTION>yyset_lineno    { ACTION_IFDEF("YY_NO_SET_LINENO", ! option_sense); }
<OPTION>yyget_in        { ACTION_IFDEF("YY_NO_GET_IN", ! option_sense); }
<OPTION>yyset_in        { ACTION_IFDEF("YY_NO_SET_IN", ! option_sense); }
<OPTION>yyget_out       { ACTION_IFDEF("YY_NO_GET_OUT", ! option_sense); }
<OPTION>yyset_out       { ACTION_IFDEF("YY_NO_SET_OUT", ! option_sense); }
<OPTION>yyget_lval      { ACTION_IFDEF("YY_NO_GET_LVAL", ! option_sense); }
<OPTION>yyset_lval      { ACTION_IFDEF("YY_NO_SET_LVAL", ! option_sense); }
<OPTION>yyget_lloc      { ACTION_IFDEF("YY_NO_GET_LLOC", ! option_sense); }
<OPTION>yyset_lloc      { ACTION_IFDEF("YY_NO_SET_LLOC", ! option_sense); }

<OPTION>outfile		{ return OPT_OUTFILE; }
<OPTION>prefix		{ return OPT_PREFIX; }
<OPTION>yyclass		{ return OPT_YYCLASS; }
<OPTION>header      { return OPT_HEADER; }

<OPTION>\"[^"\n]*\"	{
			strcpy( nmstr, yytext + 1 );
			nmstr[strlen( nmstr ) - 1] = '\0';
			return NAME;
			}

<OPTION>(([a-mo-z]|n[a-np-z])[a-zA-Z\-+]*)|.	{
			format_synerr( _( "unrecognized %%option: %s" ),
				yytext );
			BEGIN(RECOVER);
			}

<RECOVER>.*{NL}		{ ++linenum; BEGIN(INITIAL); }


<SECT2PROLOG>^"%{".*	{ ++bracelevel; yyless( 2 ); }
<SECT2PROLOG>^"%}".*	{ --bracelevel; yyless( 2 ); }

<SECT2PROLOG>^{WS}.*	{ ACTION_ECHO;	/* indented code in prolog */ }

<SECT2PROLOG>^{NOT_WS}.*	{	/* non-indented code */
			if ( bracelevel <= 0 )
				{ /* not in %{ ... %} */
				yyless( 0 );	/* put it all back */
				yy_set_bol( 1 );
				mark_prolog();
				BEGIN(SECT2);
				}
			else
				ACTION_ECHO;
			}

<SECT2PROLOG>.*		{ ACTION_ECHO; }
<SECT2PROLOG>{NL}	{ ++linenum; ACTION_ECHO; }

<SECT2PROLOG><<EOF>>		{
			mark_prolog();
			sectnum = 0;
			yyterminate(); /* to stop the parser */
			}

<SECT2>^{OPTWS}{NL}	{ ++linenum; /* allow blank lines in section 2 */ }

<SECT2>^{OPTWS}"%{"	{
			indented_code = false;
			doing_codeblock = true;
			bracelevel = 1;
                        synerr(_("percent brace action unsupported"));
			}

<SECT2>^{OPTWS}"<"	{ BEGIN(SC); return '<'; }
<SECT2>^{OPTWS}"^"	{ return '^'; }
<SECT2>\"		{ BEGIN(QUOTE); return '"'; }
<SECT2>"{"/[0-9]	{
			BEGIN(NUM);
			if ( lex_compat || posix_compat )
				return BEGIN_REPEAT_POSIX;
			else
				return BEGIN_REPEAT_FLEX;
			}
<SECT2>"$"/([ \t]|{NL})	{ return '$'; }

<SECT2>{WS}"%{"		{
			bracelevel = 1;
                        synerr(_("percent brace unsupported"));

			if ( in_rule )
				{
				doing_rule_action = true;
				in_rule = false;
				return '\n';
				}
			}
<SECT2>{WS}"|".*{NL}	{ continued_action = true; ++linenum; return '\n'; }

<SECT2>^{WS}"/*"	{
			yyless( yyleng - 2 );	/* put back '/', '*' */
			bracelevel = 0;
			continued_action = false;
			BEGIN(ACTION);
			}

<SECT2>^{WS}		{ /* allow indented rules */ }

<SECT2>{WS}		{
			/* This rule is separate from the one below because
			 * otherwise we get variable trailing context, so
			 * we can't build the scanner using -{f,F}.
			 */
			bracelevel = 0;
			continued_action = false;
			BEGIN(ACTION);

			if ( in_rule )
				{
				doing_rule_action = true;
				in_rule = false;
				return '\n';
				}
			}

<SECT2>{OPTWS}{NL}	{
			bracelevel = 0;
			continued_action = false;
			BEGIN(ACTION);
			unput( '\n' );	/* so <ACTION> sees it */

			if ( in_rule )
				{
				doing_rule_action = true;
				in_rule = false;
				return '\n';
				}
			}

<SECT2>^{OPTWS}"<<EOF>>"	|
<SECT2>"<<EOF>>"	{ return EOF_OP; }

<SECT2>^"%%".*		{
			sectnum = 3;
			BEGIN(SECT3);
			outn("/* Begin user sect3 */");
			yyterminate(); /* to stop the parser */
			}

<SECT2>"["({FIRST_CCL_CHAR}|{CCL_EXPR})({CCL_CHAR}|{CCL_EXPR})*	{
			int cclval;

			strcpy( nmstr, yytext );

			/* Check to see if we've already encountered this
			 * ccl.
			 */
			if ( (cclval = ccllookup( (Char *) nmstr )) != 0 )
				{
				if ( input() != ']' )
					synerr( _( "bad character class" ) );

				yylval = cclval;
				++cclreuse;
				return PREVCCL;
				}
			else
				{
				/* We fudge a bit.  We know that this ccl will
				 * soon be numbered as lastccl + 1 by cclinit.
				 */
				cclinstal( (Char *) nmstr, lastccl + 1 );

				/* Push back everything but the leading bracket
				 * so the ccl can be rescanned.
				 */
				yyless( 1 );

				BEGIN(FIRSTCCL);
				return '[';
				}
			}

<SECT2>"{"{NAME}"}"[ \t\n\f\r\v]?	 {
			register Char *nmdefptr;
            int end_is_ws, end_ch;
            
            end_ch = yytext[yyleng-1];
            end_is_ws = end_ch != '}' ? 1 : 0;

			strcpy( nmstr, yytext + 1 );
			nmstr[yyleng - 2 - end_is_ws] = '\0';  /* chop trailing brace */

			if ( (nmdefptr = ndlookup( nmstr )) == 0 )
				format_synerr(
					_( "undefined definition {%s}" ),
						nmstr );

			else
				{ /* push back name surrounded by ()'s */
				int len = strlen( (char *) nmdefptr );
                if (end_is_ws)
                    unput(end_ch);

				if ( lex_compat || nmdefptr[0] == '^' ||
				     (len > 0 && nmdefptr[len - 1] == '$')
                     || end_is_ws)
					{ /* don't use ()'s after all */
					PUT_BACK_STRING((char *) nmdefptr, 0);

					if ( nmdefptr[0] == '^' )
						BEGIN(CARETISBOL);
					}

				else
					{
					unput(')');
					PUT_BACK_STRING((char *) nmdefptr, 0);
					unput('(');
					}
				}
			}

<SECT2>[/|*+?.(){}]	{ return (unsigned char) yytext[0]; }
<SECT2>.		{ RETURNCHAR; }


<SC>{OPTWS}{NL}{OPTWS}	{ ++linenum;	/* Allow blank lines & continuations */ }
<SC>[,*]		{ return (unsigned char) yytext[0]; }
<SC>">"		{ BEGIN(SECT2); return '>'; }
<SC>">"/^		{ BEGIN(CARETISBOL); return '>'; }
<SC>{SCNAME}	{ RETURNNAME; }
<SC>.		{
			format_synerr( _( "bad <start condition>: %s" ),
				yytext );
			}

<CARETISBOL>"^"		{ BEGIN(SECT2); return '^'; }


<QUOTE>[^"\n]		{ RETURNCHAR; }
<QUOTE>\"		{ BEGIN(SECT2); return '"'; }

<QUOTE>{NL}		{
			synerr( _( "missing quote" ) );
			BEGIN(SECT2);
			++linenum;
			return '"';
			}


<FIRSTCCL>"^"/[^\]\n-]	{ BEGIN(CCL); return '^'; }
<FIRSTCCL>"^"/("-"|"]")	{ return '^'; }
<FIRSTCCL>.		{ BEGIN(CCL); RETURNCHAR; }

<CCL>-/[^\]\n]	{ return '-'; }
<CCL>[^\]\n]		{ RETURNCHAR; }
<CCL>"]"		{ BEGIN(SECT2); return ']'; }
<CCL>.|{NL}		{
			synerr( _( "bad character class" ) );
			BEGIN(SECT2);
			return ']';
			}

<FIRSTCCL,CCL>"[:alnum:]"	{ BEGIN(CCL); return CCE_ALNUM; }
<FIRSTCCL,CCL>"[:alpha:]"	{ BEGIN(CCL); return CCE_ALPHA; }
<FIRSTCCL,CCL>"[:blank:]"	{ BEGIN(CCL); return CCE_BLANK; }
<FIRSTCCL,CCL>"[:cntrl:]"	{ BEGIN(CCL); return CCE_CNTRL; }
<FIRSTCCL,CCL>"[:digit:]"	{ BEGIN(CCL); return CCE_DIGIT; }
<FIRSTCCL,CCL>"[:graph:]"	{ BEGIN(CCL); return CCE_GRAPH; }
<FIRSTCCL,CCL>"[:lower:]"	{ BEGIN(CCL); return CCE_LOWER; }
<FIRSTCCL,CCL>"[:print:]"	{ BEGIN(CCL); return CCE_PRINT; }
<FIRSTCCL,CCL>"[:punct:]"	{ BEGIN(CCL); return CCE_PUNCT; }
<FIRSTCCL,CCL>"[:space:]"	{ BEGIN(CCL); return CCE_SPACE; }
<FIRSTCCL,CCL>"[:upper:]"	{ BEGIN(CCL); return CCE_UPPER; }
<FIRSTCCL,CCL>"[:xdigit:]"	{ BEGIN(CCL); return CCE_XDIGIT; }
<FIRSTCCL,CCL>{CCL_EXPR}	{
			format_synerr(
				_( "bad character class expression: %s" ),
					yytext );
			BEGIN(CCL); return CCE_ALNUM;
			}

<NUM>[0-9]+	{
			yylval = myctoi( yytext );
			return NUMBER;
			}

<NUM>","		{ return ','; }
<NUM>"}"		{
			BEGIN(SECT2);
			if ( lex_compat || posix_compat )
				return END_REPEAT_POSIX;
			else
				return END_REPEAT_FLEX;
			}

<NUM>.		{
			synerr( _( "bad character inside {}'s" ) );
			BEGIN(SECT2);
			return '}';
			}

<NUM>{NL}		{
			synerr( _( "missing }" ) );
			BEGIN(SECT2);
			++linenum;
			return '}';
			}



<ACTION>"{"		{ ACTION_ECHO; ++bracelevel; }
<ACTION>"}"		{ ACTION_ECHO; --bracelevel; }
<ACTION>[^a-zA-Z_\{\}\"'/\n]+	{ ACTION_ECHO; }
<ACTION>{NAME}		{ ACTION_ECHO; }
<ACTION>"'"([^'\\\n]|\\.)*"'"	{ ACTION_ECHO; /* character constant */ }
<ACTION>\"		{ ACTION_ECHO; BEGIN(ACTION_STRING); }
<ACTION>{NL}		{
			++linenum;
			ACTION_ECHO;
			if ( bracelevel == 0 )
				{
				if ( doing_rule_action )
					add_action( "\tYY_BREAK\n" );

				doing_rule_action = false;
				BEGIN(SECT2);
				}
			}
<ACTION>.		{ ACTION_ECHO; }

<ACTION_STRING>[^"\\\n]+	{ ACTION_ECHO; }
<ACTION_STRING>\\.		{ ACTION_ECHO; }
<ACTION_STRING>{NL}		{ ++linenum; ACTION_ECHO; BEGIN(ACTION); }
<ACTION_STRING>\"		{ ACTION_ECHO; BEGIN(ACTION); }
<ACTION_STRING>.		{ ACTION_ECHO; }

<COMMENT,ACTION,ACTION_STRING><<EOF>>	{
			synerr( _( "EOF encountered inside an action" ) );
			yyterminate();
			}


<SECT2,QUOTE,FIRSTCCL,CCL>{ESCSEQ}	{
			yylval = myesc( (Char *) yytext );

			if ( YYSTATE == FIRSTCCL )
				BEGIN(CCL);

			return CHAR;
			}


<SECT3>.*(\n?)		{ ECHO; }
<SECT3><<EOF>>		{ sectnum = 0; yyterminate(); }

.|\n                        { format_synerr( _( "bad character: %s" ), yytext ); }

%%


int yywrap()
	{
	if ( --num_input_files > 0 )
		{
		set_input_file( *++input_files );
		return 0;
		}

	else
		return 1;
	}


/* set_input_file - open the given file (if NULL, stdin) for scanning */

void set_input_file( file )
char *file;
	{
	if ( file && strcmp( file, "-" ) )
		{
		infilename = copy_string( file );
		yyin = fopen( infilename, "r" );

		if ( yyin == NULL )
			lerrsf( _( "can't open %s" ), file );
		}

	else
		{
		yyin = stdin;
		infilename = copy_string( "<stdin>" );
		}

	linenum = 1;
	}


/* Wrapper routines for accessing the scanner's malloc routines. */

void *flex_alloc( size )
size_t size;
	{
	return (void *) malloc( size );
	}

void *flex_realloc( ptr, size )
void *ptr;
size_t size;
	{
	return (void *) realloc( ptr, size );
	}

void flex_free( ptr )
void *ptr;
	{
	if ( ptr )
		free( ptr );
	}

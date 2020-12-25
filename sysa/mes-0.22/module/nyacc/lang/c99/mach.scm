;;; lang/c99/mach.scm - C parser grammer

;; Copyright (C) 2015-2018 Matthew R. Wette
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this library; if not, see <http://www.gnu.org/licenses/>.

;;; Notes:

;; This is a C parser, based on ISO-C99, with comments and CPP statements.

;; Currentl K&R function definitions are not supported:
;;   int f(x) int x; { ... } => syntax error
;; because they lead to an ambiguous grammar, I believe:
;;   int f(x) __attribute__((__static__)) int x; { ... }

;; See also:
;;   http://www.quut.com/c/ANSI-C-grammar-y.html - C11 grammar
;;   https://gcc.gnu.org/onlinedocs/gcc/C-Extensions.html#C-Extensions

;;; Code:

(define-module (nyacc lang c99 mach)
  #:export (c99-spec c99-mach c99x-spec c99x-mach gen-c99-files)
  #:use-module (nyacc lang c99 cpp)
  #:use-module (nyacc lang util)
  #:use-module (nyacc lalr)
  #:use-module (nyacc parse)
  #:use-module (nyacc lex)
  #:use-module (nyacc util)
  #:use-module ((srfi srfi-43) #:select (vector-map)))

;; @deffn {Variable} c99-spec
;; This variable is the specification a-list for the hacked ISO C99 language.
;; Well, actually, this does not produce pure C99 spec: it has been extended
;; to be able be used for practical purposes so it now parses forms like
;; @code{__asm__} and @code{__attribute__}.
;; Run this through @code{make-lalr-machine} to get an a-list for the
;; automaton.  The grammar is modified to parse CPP statements and comments.
;; The output of the end parser will be a SXML tree (w/o the @code{*TOP*} node.
;; @end deffn
(define c99-spec
  (lalr-spec
   (notice (string-append "Copyright (C) 2016-2018 Matthew R. Wette"
			  license-lgpl3+))

   (prec< 'then "else")	       ; "then/else" SR-conflict resolution
   (prec< 'imp		       ; "implied type" SR-conflict resolution
	  (nonassoc "char" "short" "int" "long" "_Fract" "_Accum" "_Sat")
	  (nonassoc "float" "double" "_Complex"))
   (prec< 'shift-on-attr		; living on the edge ...
	  (nonassoc "__attribute__" "__packed__" "__aligned__" "__alignof__")
	  'reduce-on-attr
	  'reduce-on-semi
	  (nonassoc "*" "(" '$ident))

   (start translation-unit)
   (grammar

    ;; === expressions ========================================================

    (primary-expression			; S 6.5.1
     (identifier ($$ `(p-expr ,$1)))
     (constant ($$ `(p-expr ,$1)))
     (string-literal ($$ `(p-expr ,$1)))
     ("(" expression ")" ($$ $2))
     ("(" "{" ($$ (cpi-push)) block-item-list ($$ (cpi-pop)) "}" ")"
      ($$ `(stmt-expr (@ (extension "GNUC")) ,$4))))

    (postfix-expression			; S 6.5.2
     (primary-expression)
     (postfix-expression "[" expression "]" ($$ `(array-ref ,$3 ,$1)))
     (postfix-expression "(" argument-expression-list ")"
			 ($$ `(fctn-call ,$1 ,(tl->list $3))))
     (postfix-expression "(" ")" ($$ `(fctn-call ,$1 (expr-list))))
     (postfix-expression "." identifier ($$ `(d-sel ,$3 ,$1)))
     (postfix-expression "->" identifier ($$ `(i-sel ,$3 ,$1)))
     (postfix-expression "++" ($$ `(post-inc ,$1)))
     (postfix-expression "--" ($$ `(post-dec ,$1)))
     ("(" type-name ")" "{" initializer-list "}"
      ($$ `(comp-lit ,$2 ,(tl->list $5))))
     ("(" type-name ")" "{" initializer-list "," "}"
      ($$ `(comp-lit ,$2 ,(tl->list $5)))))

    (argument-expression-list
     (assignment-expression ($$ (make-tl 'expr-list $1)))
     (argument-expression-list "," assignment-expression ($$ (tl-append $1 $3)))
     ;; The following is a hack to deal with using abstract declarations
     ;; as arguments to CPP macros (e.g., see offsetof in <stddef.h>).
     (arg-expr-hack ($$ (make-tl 'expr-list $1)))
     (argument-expression-list "," arg-expr-hack ($$ (tl-append $1 $3))))
    (arg-expr-hack
     (declaration-specifiers
      abstract-declarator ($$ `(param-decl ,(tl->list $1) $2)))
     (declaration-specifiers ($$ `(param-decl ,(tl->list $1)))))

    (unary-expression
     (postfix-expression)		; S 6.5.3
     ("++" unary-expression ($$ `(pre-inc ,$2)))
     ("--" unary-expression ($$ `(pre-dec ,$2)))
     (unary-operator cast-expression ($$ (list $1 $2)))
     ("sizeof" unary-expression ($$ `(sizeof-expr ,$2)))
     ("sizeof" "(" type-name ")" ($$ `(sizeof-type ,$3)))
     )
    (unary-operator ("&" ($$ 'ref-to)) ("*" ($$ 'de-ref))
		    ("+" ($$ 'pos)) ("-" ($$ 'neg))
		    ("~" ($$ 'bitwise-not)) ("!" ($$ 'not)))

    (cast-expression			; S 6.5.4
     (unary-expression)
     ("(" type-name ")" cast-expression ($$ `(cast ,$2 ,$4))))

    (multiplicative-expression		; S 6.5.5
     (cast-expression)
     (multiplicative-expression "*" cast-expression ($$ `(mul ,$1 ,$3)))
     (multiplicative-expression "/" cast-expression ($$ `(div ,$1 ,$3)))
     (multiplicative-expression "%" cast-expression ($$ `(mod ,$1 ,$3))))

    (additive-expression		; S 6.5.6
     (multiplicative-expression)
     (additive-expression "+" multiplicative-expression ($$ `(add ,$1 ,$3)))
     (additive-expression "-" multiplicative-expression ($$ `(sub ,$1 ,$3))))

    (shift-expression			; S 6.5.7
     (additive-expression)
     (shift-expression "<<" additive-expression ($$ `(lshift ,$1 ,$3)))
     (shift-expression ">>" additive-expression ($$ `(rshift ,$1 ,$3))))

    (relational-expression		; S 6.5.8
     (shift-expression)
     (relational-expression "<" shift-expression ($$ `(lt ,$1 ,$3)))
     (relational-expression ">" shift-expression ($$ `(gt ,$1 ,$3)))
     (relational-expression "<=" shift-expression ($$ `(le ,$1 ,$3)))
     (relational-expression ">=" shift-expression ($$ `(ge ,$1 ,$3))))
    
    (equality-expression		; S 6.5.9
     (relational-expression)
     (equality-expression "==" relational-expression ($$ `(eq ,$1 ,$3)))
     (equality-expression "!=" relational-expression ($$ `(ne ,$1 ,$3))))

    ;; called AND-expression
    (bitwise-and-expression		; S 6.5.10
     (equality-expression)
     (bitwise-and-expression "&" equality-expression
			     ($$ `(bitwise-and ,$1 ,$3))))

    ;; called exclusive-OR-expression
    (bitwise-xor-expression		; S 6.5.11
     (bitwise-and-expression)
     (bitwise-xor-expression "^" bitwise-and-expression
			     ($$ `(bitwise-xor ,$1 ,$3))))

    ;; called inclusive-OR-expression
    (bitwise-or-expression		; S 6.5.12
     (bitwise-xor-expression)
     (bitwise-or-expression "|" bitwise-xor-expression
			    ($$ `(bitwise-or ,$1 ,$3))))

    (logical-and-expression		; S 6.5.13
     (bitwise-or-expression)
     (logical-and-expression "&&" bitwise-or-expression
			     ($$ `(and ,$1 ,$3))))

    (logical-or-expression		; 6.5.14
     (logical-and-expression)
     (logical-or-expression "||" logical-and-expression
			    ($$ `(or ,$1 ,$3))))
    
    (conditional-expression
     (logical-or-expression)
     (logical-or-expression "?" expression ":" conditional-expression
			    ($$ `(cond-expr ,$1 ,$3 ,$5))))

    (assignment-expression		; S 6.5.16
     (conditional-expression)
     (unary-expression assignment-operator assignment-expression
		       ($$ `(assn-expr ,$1 (op ,$2) ,$3))))
    (assignment-operator
     ("=") ("+=") ("-=") ("*=") ("/=") ("%=")
     ("<<=") (">>=") ("&=") ("^=") ("|="))

    (expression				; S 6.5.17
     (assignment-expression)
     (expression "," assignment-expression
		 ($$ (if (eqv? 'comma-expr (sx-tag $1))
			 (append $1 (list $3))
			 `(comma-expr ,$1 ,$3)))))

    (constant-expression		; S 6.6
     (conditional-expression))

    ;; === declarations

    ;; TODO: check if we should move attributes or trap attribute-only spec's
    (declaration			; S 6.7
     (declaration-no-comment ";")
     (declaration-no-comment ";" code-comment ($$ (sx-attr-add $1 $3))))

    (declaration-no-comment
     (declaration-specifiers
      init-declarator-list
      ($$ (save-typenames `(decl ,$1 ,$2))))
     (declaration-specifiers
      ($$ `(decl ,$1))))

    ;; --- declaration specifiers

    (declaration-specifiers		; S 6.7
     (declaration-specifiers-1 ($$ (process-specs (tl->list $1)))))
    (declaration-specifiers-1
     ;; storage-class-specifiers
     (storage-class-specifier
      ($prec 'shift-on-attr) ($$ (make-tl 'decl-spec-list $1)))
     (storage-class-specifier declaration-specifiers-1 ($$ (tl-insert $2 $1)))
     ;; type-specifiers
     (type-specifier
      ($prec 'reduce-on-attr) ($$ (make-tl 'decl-spec-list $1)))
     (type-specifier declaration-specifiers-1 ($$ (tl-insert $2 $1)))
     ;; type-qualifiers
     (type-qualifier
      ($prec 'shift-on-attr) ($$ (make-tl 'decl-spec-list $1)))
     (type-qualifier declaration-specifiers-1 ($$ (tl-insert $2 $1)))
     ;; function-specifiers
     (function-specifier
      ($prec 'reduce-on-attr) ($$ (make-tl 'decl-spec-list $1)))
     (function-specifier declaration-specifiers-1 ($$ (tl-insert $2 $1)))
     ;; attribute-specifiers
     (attribute-specifier
      ($prec 'reduce-on-semi) ($$ (make-tl 'decl-spec-list $1)))
     (attribute-specifier declaration-specifiers-1 ($$ (tl-insert $2 $1))))

    (storage-class-specifier		; S 6.7.1
     ("auto" ($$ '(stor-spec (auto))))
     ("extern" ($$ '(stor-spec (extern))))
     ("register" ($$ '(stor-spec (register))))
     ("static" ($$ '(stor-spec (static))))
     ("typedef" ($$ '(stor-spec (typedef)))))

    ;; I have created fixed-, float- and complex- type specifiers to capture
    ;; combinations like "short int" "long long" etc.
    (type-specifier			; S 6.7.2
     ("void" ($$ '(type-spec (void))))
     (fixed-type-specifier ($$ `(type-spec ,$1)))
     (float-type-specifier ($$ `(type-spec ,$1)))
     (fixpt-type-specifier ($$ `(type-spec ,$1)))
     ("_Bool" ($$/ref 's5.1.5-01 '(type-spec (fixed-type "_Bool"))))
     (complex-type-specifier ($$ `(type-spec ,$1)))
     (struct-or-union-specifier ($$ `(type-spec ,$1)))
     (enum-specifier ($$ `(type-spec ,$1)))
     (typedef-name ($$ `(type-spec ,$1))))

    (fixed-type-specifier
     ("short" ($prec 'imp) ($$ '(fixed-type "short")))
     ("short" "int" ($$ '(fixed-type "short int")))
     ("signed" "short" ($prec 'imp) ($$ '(fixed-type "signed short")))
     ("signed" "short" "int" ($$ '(fixed-type "signed short int")))
     ("int" ($$ '(fixed-type "int")))
     ("signed" ($prec 'imp) ($$ '(fixed-type "signed")))
     ("signed" "int" ($$ '(fixed-type "signed int")))
     ("long" ($prec 'imp) ($$ '(fixed-type "long")))
     ("long" "int" ($$ '(fixed-type "long int")))
     ("signed" "long" ($prec 'imp) ($$ '(fixed-type "signed long")))
     ("signed" "long" "int" ($$ '(fixed-type "signed long int")))
     ("long" "long" ($prec 'imp) ($$ '(fixed-type "long long")))
     ("long" "long" "int" ($$ '(fixed-type "long long int")))
     ("signed" "long" "long" ($prec 'imp)
      ($$ '(fixed-type "signed long long")))
     ("signed" "long" "long" "int" ($$ '(fixed-type "signed long long int")))
     ("unsigned" "short" "int" ($$ '(fixed-type "unsigned short int")))
     ("unsigned" "short" ($prec 'imp) ($$ '(fixed-type "unsigned short")))
     ("unsigned" "int" ($$ '(fixed-type "unsigned int")))
     ("unsigned" ($prec 'imp) ($$ '(fixed-type "unsigned")))
     ("unsigned" "long" "int" ($$ '(fixed-type "unsigned long")))
     ("unsigned" "long" ($prec 'imp) ($$ '(fixed-type "unsigned long")))
     ("unsigned" "long" "long" "int"
      ($$ '(fixed-type "unsigned long long int")))
     ("unsigned" "long" "long" ($prec 'imp)
      ($$ '(fixed-type "unsigned long long")))
     ("char" ($$ '(fixed-type "char")))
     ("signed" "char" ($$ '(fixed-type "signed char")))
     ("unsigned" "char" ($$ '(fixed-type "unsigned char"))))

    (float-type-specifier
     ("float" ($prec 'imp) ($$ '(float-type "float")))
     ("double" ($prec 'imp) ($$ '(float-type "double")))
     ("long" "double" ($$ '(float-type "long double"))))

    (complex-type-specifier
     ("_Complex" ($$ '(complex-type "_Complex")))
     ("float" "_Complex" ($$ '(complex-type "float _Complex")))
     ("double" "_Complex" ($$ '(complex-type "double _Complex")))
     ("long" "double" "_Complex" ($$ '(complex-type "long double _Complex"))))

    (fixpt-type-specifier
     ;; http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2001/n1290.pdf
     ("short" "_Fract" ($$ '(fixpt-type "short _Fract")))
     ("_Fract" ($$ '(fixpt-type "_Fract")))
     ("long" "_Fract" ($$ '(fixpt-type "long _Fract")))
     ("signed" "short" "_Fract" ($$ '(fixpt-type "signd short _Fract")))
     ("signed" "_Fract" ($$ '(fixpt-type "signed _Fract")))
     ("signed" "long _Fract" ($$ '(fixpt-type "signed long _Fract")))
     ("unsigned" "short" "_Fract" ($$ '(fixpt-type "unsigned short _Fract")))
     ("unsigned" "_Fract" ($$ '(fixpt-type "unsigned _Fract")))
     ("unsigned" "long _Fract" ($$ '(fixpt-type "unsigned long _Fract")))
     ("short" "_Accum" ($$ '(fixpt-type "short _Accum")))
     ("_Accum" ($$ '(fixpt-type "_Accum")))
     ("long _Accum" ($$ '(fixpt-type "long _Accum")))
     ("signed" "short" "_Accum" ($$ '(fixpt-type "signd short _Accum")))
     ("signed" "_Accum" ($$ '(fixpt-type "signed _Accum")))
     ("signed" "long" "_Accum" ($$ '(fixpt-type "signed long _Accum")))
     ("unsigned" "short" "_Accum" ($$ '(fixpt-type "unsigned short _Accum")))
     ("unsigned" "_Accum" ($$ '(fixpt-type "unsigned _Accum")))
     ("unsigned" "long" "_Accum" ($$ '(fixpt-type "unsigned long _Accum")))
     ("_Sat" "short" "_Fract" ($$ '(fixpt-type "_Sat short _Fract")))
     ("_Sat" "_Fract" ($$ '(fixpt-type "_Sat _Fract")))
     ("_Sat" "long" "_Fract" ($$ '(fixpt-type "_Sat long _Fract")))
     ("_Sat" "signed" "short" "_Fract"
      ($$ '(fixpt-type "_Sat signd short _Fract")))
     ("_Sat" "signed" "_Fract" ($$ '(fixpt-type "_Sat signed _Fract")))
     ("_Sat" "signed" "long _Fract"
      ($$ '(fixpt-type "_Sat signed long _Fract")))
     ("_Sat" "unsigned" "short" "_Fract"
      ($$ '(fixpt-type "_Sat unsigned short _Fract")))
     ("_Sat" "unsigned" "_Fract" ($$ '(fixpt-type "_Sat unsigned _Fract")))
     ("_Sat" "unsigned" "long" "_Fract"
      ($$ '(fixpt-type "_Sat unsigned long _Fract")))
     ("_Sat" "short" "_Accum" ($$ '(fixpt-type "_Sat short _Accum")))
     ("_Sat" "_Accum" ($$ '(fixpt-type "_Sat _Accum")))
     ("_Sat" "long" "_Accum" ($$ '(fixpt-type "_Sat long _Accum")))
     ("_Sat" "signed" "short" "_Accum"
      ($$ '(fixpt-type "_Sat signd short _Accum")))
     ("_Sat" "signed" "_Accum" ($$ '(fixpt-type "_Sat signed _Accum")))
     ("_Sat" "signed" "long" "_Accum"
      ($$ '(fixpt-type "_Sat signed long _Accum")))
     ("_Sat" "unsigned" "short" "_Accum"
      ($$ '(fixpt-type "_Sat unsigned short _Accum")))
     ("_Sat" "unsigned" "_Accum" ($$ '(fixpt-type "_Sat unsigned _Accum")))
     ("_Sat" "unsigned" "long" "_Accum"
      ($$ '(fixpt-type "_Sat unsigned long _Accum"))))

    ;; This one modified: split out struct-or-union = "struct"|"union"
    (struct-or-union-specifier
     ("struct" opt-attr-specs ident-like "{" struct-declaration-list "}"
      ($$ (sx-join* 'struct-def $2 $3 (tl->list $5))))
     ("struct" opt-attr-specs "{" struct-declaration-list "}"
      ($$ (sx-join* 'struct-def $2 (tl->list $4))))
     ("struct" opt-attr-specs ident-like ($$ (sx-join* 'struct-ref $1 $3)))
     ("union" opt-attr-specs ident-like "{" struct-declaration-list "}"
      ($$ (sx-join* 'union-def $2 $3 (tl->list $5))))
     ("union" opt-attr-specs "{" struct-declaration-list "}"
      ($$ (sx-join* 'union-def $2 (tl->list $4))))
     ("union" opt-attr-specs ident-like ($$ (sx-join* 'union-ref $2 $3))))

    ;; because name following struct/union can be identifier or typeref:
    (ident-like
     (identifier)
     (typedef-name ($$ `(ident ,(sx-ref $1 1)))))

    (opt-attr-specs
     ($empty)
     (attribute-specifiers ($$ `(@ ,(attrl->attrs $1)))))

    ;; Calling this field-list in the parse tree.
    (struct-declaration-list		; S 6.7.2.1
     (struct-declaration ($$ (make-tl 'field-list $1)))
     (lone-comment ($$ (make-tl 'field-list $1)))
     (struct-declaration-list struct-declaration ($$ (tl-append $1 $2)))
     (struct-declaration-list lone-comment ($$ (tl-append $1 $2)))
     ;; Not in C99, but allowed by GNU, I believe:
     (";" ($$ (make-tl 'field-list)))
     (struct-declaration-list ";" ($$ $1)))

    (struct-declaration			; S 6.7.2.1
     (struct-declaration-no-comment ";")
     (struct-declaration-no-comment ";" code-comment ($$ (sx-attr-add $1 $3))))
    (struct-declaration-no-comment
     (specifier-qualifier-list
      struct-declarator-list ($$ `(comp-decl ,$1 ,(tl->list $2))))
     (specifier-qualifier-list ($$ `(comp-decl ,$1)))) ;; <= anonymous
    
    (specifier-qualifier-list		; S 6.7.2.1
     (specifier-qualifier-list-1 ($$ (process-specs (tl->list $1)))))
    (specifier-qualifier-list-1
     (type-specifier ($$ (make-tl 'decl-spec-list $1)))
     (type-specifier specifier-qualifier-list-1 ($$ (tl-insert $2 $1)))
     (type-qualifier ($$ (make-tl 'decl-spec-list $1)))
     (type-qualifier specifier-qualifier-list-1 ($$ (tl-insert $2 $1)))
     (attribute-specifier ($$ (make-tl 'decl-spec-list $1)))
     (attribute-specifier specifier-qualifier-list-1 ($$ (tl-insert $2 $1))))

    (specifier-qualifier-list/no-attr
     (specifier-qualifier-list/no-attr-1 ($$ (tl->list $1))))
    (specifier-qualifier-list/no-attr-1
     (type-specifier ($$ (make-tl 'decl-spec-list $1)))
     (type-specifier specifier-qualifier-list/no-attr-1 ($$ (tl-insert $2 $1)))
     (type-qualifier ($$ (make-tl 'decl-spec-list $1)))
     (type-qualifier specifier-qualifier-list/no-attr-1 ($$ (tl-insert $2 $1))))

    (struct-declarator-list		; S 6.7.2.1
     (struct-declarator ($$ (make-tl 'comp-declr-list $1)))
     (struct-declarator-list "," struct-declarator ($$ (tl-append $1 $3)))
     (struct-declarator-list "," attribute-specifiers
			     struct-declarator ($$ (tl-append $1 $3 $4))))

    (struct-declarator			; S 6.7.2.1
     (struct-declarator-1 ($$ (process-declr $1))))
    (struct-declarator-1
     (declarator ($$ `(comp-declr ,$1)))
     (declarator attribute-specifiers ($$ `(comp-declr ,$1 ,$2)))
     (declarator ":" constant-expression
		 ($$ `(comp-declr (bit-field ,$1 ,$3))))
     (":" constant-expression ($$ `(comp-declr (bit-field ,$2)))))

    (enum-specifier			; S 6.7.2.2
     ("enum" ident-like "{" enumerator-list "}"
      ($$ `(enum-def ,$2 ,(tl->list $4))))
     ("enum" ident-like "{" enumerator-list "," "}"
      ($$ `(enum-def ,$2 ,(tl->list $4))))
     ("enum" "{" enumerator-list "}" ($$ `(enum-def ,(tl->list $3))))
     ("enum" "{" enumerator-list "," "}" ($$ `(enum-def ,(tl->list $3))))
     ("enum" ident-like ($$ `(enum-ref ,$2))))

    ;; keeping old enum-def-list in parse tree
    (enumerator-list			; S 6.7.2.2
     (enumerator ($$ (make-tl 'enum-def-list $1)))
     (enumerator-list "," enumerator ($$ (tl-append $1 $3))))

    ;; had to change enumeration-constant => identifier
    (enumerator				; S 6.7.2.2
     (identifier ($$ `(enum-defn ,$1)))
     (identifier attribute-specifiers ($$ `(enum-defn ,$1 ,$2)))
     (identifier "=" constant-expression ($$ `(enum-defn ,$1 ,$3))))

    (type-qualifier
     ("const" ($$ `(type-qual ,$1)))
     ("volatile" ($$ `(type-qual ,$1)))
     ("restrict" ($$ `(type-qual ,$1))))

    (function-specifier
     ("inline" ($$ `(fctn-spec ,$1)))
     ("_Noreturn" ($$ `(fctn-spec ,$1))))
    
    ;; Support for __attribute__(( ... )).  See the gcc documentation.
    ;; The documentation does not seem rigourous about defining where the
    ;; attribute specifier can appear.  This is my best attempt.  MW 2018
    ;; https://gcc.gnu.org/onlinedocs/gcc-8.2.0/gcc/Attribute-Syntax.html
    ;; https://gcc.gnu.org/onlinedocs/gcc-8.2.0/gcc/Type-Attributes.html
    ;; https://gcc.gnu.org/onlinedocs/gcc-8.2.0/gcc/Variable-Attributes.html
    ;; https://gcc.gnu.org/onlinedocs/gcc-8.2.0/gcc/Function-Attributes.html
    
    (attribute-specifiers
     (attribute-specifier ($prec 'reduce-on-attr))
     (attribute-specifiers attribute-specifier ($$ (append $1 (cdr $2)))))

    ;; (attributes (attribute "__static__") (attribute aligned(8)" ...)
    (attribute-specifier
     ("__attribute__" "(" "(" attribute-list ")" ")" ($$ $4))
     (attr-name ($$ `(attribute-list (attribute ,$1)))))
    (attr-name
     ("__packed__" ($$ '(ident "__packed__")))
     ("__aligned__" ($$ '(ident "__aligned__")))
     ("__alignof__" ($$ '(ident "__alignof__"))))

    (attribute-list (attribute-list-1 ($$ (tl->list $1))))
    (attribute-list-1
     (attribute ($$ (make-tl 'attribute-list $1)))
     (attribute-list-1 "," attribute ($$ (tl-append $1 $3)))
     (attribute-list-1 "," ($$ $1)))

    (attribute
     (attr-word ($$ `(attribute ,$1)))
     (attr-word "(" attr-expr-list ")" ($$ `(attribute ,$1 ,$3)))
     ("const" ($$ `(attribute (ident "const")))))

    (attr-word
     (attr-name)
     (identifier))

    (attr-expr-list
     (attr-expr-list-1 ($$ (tl->list $1))))

    (attr-expr-list-1
     (attribute-expr ($$ (make-tl 'attr-expr-list $1)))
     (attr-expr-list-1 "," attribute-expr ($$ (tl-append $1 $3))))

    (attribute-expr
     (type-name)
     ($fixed ($$ `(fixed ,$1)))
     (string-literal)
     (identifier)
     (attr-word "(" attr-expr-list ")" ($$ `(attribute ,$1 ,$3)))) ;; ???

    ;; --- declarators

    (init-declarator-list		; S 6.7
     (init-declarator-list-1 ($$ (tl->list $1))))
    (init-declarator-list-1
     (init-declarator ($$ (make-tl 'init-declr-list $1)))
     (init-declarator-list-1 "," init-declarator ($$ (tl-append $1 $3)))
     (init-declarator-list-1 "," attribute-specifiers
			     init-declarator ($$ (tl-append $1 $3 $4))))

    (init-declarator			; S 6.7
     (init-declarator-1 ($$ (process-declr $1))))
    (init-declarator-1
     (declarator ($$ `(init-declr ,$1)))
     (declarator "=" initializer ($$ `(init-declr ,$1 ,$3)))
     (declarator asm-expression ($$ `(init-declr ,$1 ,$2)))
     (declarator asm-expression "=" initializer ($$ `(init-declr ,$1 ,$2 ,$4)))
     (declarator attribute-specifiers ($$ `(init-declr ,$1 ,$2)))
     (declarator attribute-specifiers "=" initializer
		 ($$ `(init-declr ,$1 ,$2 ,$4)))
     (declarator asm-expression attribute-specifiers
		 ($$ `(init-declr ,$1 ,$2 ,$3))))

    (declarator
     (pointer direct-declarator ($$ `(ptr-declr ,$1 ,$2)))
     (direct-declarator))

    (pointer				; S 6.7.6
     ("*" type-qualifier-list pointer ($$ `(pointer ,$2 ,$3)))
     ("*" type-qualifier-list ($$ `(pointer ,$2)))
     ("*" pointer ($$ `(pointer ,$2)))
     ("*" attribute-specifiers pointer ($$ `(pointer ,$3)))
     ("*" ($$ '(pointer))))

    (direct-declarator			; S 6.7.6
     (identifier ($$ $1))
     ;;(ident-like ($$ $1))
     ("(" declarator ")" ($$ `(scope ,$2)))
     ("(" attribute-specifier declarator ")" ($$ `(scope ,$2)))
     (direct-declarator
      "[" type-qualifier-list assignment-expression "]"
      ($$ `(array-of ,$1 ,$3 ,$4)))
     (direct-declarator
      "[" type-qualifier-list "]" ($$ `(array-of ,$1 ,$3)))
     (direct-declarator
      "[" assignment-expression "]" ($$ `(array-of ,$1 ,$3)))
     (direct-declarator
      "[" "]" ($$ `(array-of ,$1)))
     (direct-declarator
      "[" "static" type-qualifier-list assignment-expression "]"
      ($$ `(array-of ,$1 ,$4 ,$5))) ;; FIXME $4 needs "static" added
     (direct-declarator
      "[" type-qualifier-list "static" assignment-expression "]"
      ($$ `(array-of ,$1 ,4 ,$5))) ;; FIXME $4 needs "static" added
     (direct-declarator
      "[" type-qualifier-list "*" "]"	; variable length array
      ($$ `(array-of ,$1 ,$3 (var-len))))
     (direct-declarator
      "[" "*" "]"			; variable length array
      ($$ `(array-of ,$1 (var-len))))
     (direct-declarator
      "(" parameter-type-list ")" ($$ `(ftn-declr ,$1 ,$3)))
     (direct-declarator
      "(" identifier-list ")" ($$ `(ftn-declr ,$1 ,$3)))
     (direct-declarator
      "(" ")" ($$ `(ftn-declr ,$1 (param-list)))))

    (type-qualifier-list
     (type-qualifier-list-1 ($$ (tl->list $1))))
    (type-qualifier-list-1
     (type-qualifier ($$ (make-tl 'type-qual-list $1)))
     (type-qualifier-list-1 type-qualifier ($$ (tl-append $1 $2))))

    (parameter-type-list
     (parameter-list ($$ (tl->list $1)))
     (parameter-list "," "..." ($$ (tl->list (tl-append $1 '(ellipsis))))))

    (parameter-list
     (parameter-declaration ($$ (make-tl 'param-list $1)))
     (parameter-list "," parameter-declaration ($$ (tl-append $1 $3))))

    (parameter-declaration
     (declaration-specifiers
      declarator ($$ `(param-decl ,$1 (param-declr ,$2))))
     (declaration-specifiers
      abstract-declarator ($$ `(param-decl ,$1 (param-declr ,$2))))
     (declaration-specifiers
      ($$ `(param-decl ,$1))))

    (identifier-list
     (identifier-list-1 ($$ (tl->list $1))))
    (identifier-list-1
     (identifier ($$ (make-tl 'ident-list $1)))
     (identifier-list-1 "," identifier ($$ (tl-append $1 $3))))

    (type-name				; S 6.7.6
     ;; e.g., (foo_t *)
     (specifier-qualifier-list/no-attr abstract-declarator
				       ($$ `(type-name ,$1 ,$2)))
     ;; e.g., (int)
     (declaration-specifiers ($$ `(type-name ,$1))))

    (abstract-declarator		; S 6.7.6
     (pointer direct-abstract-declarator ($$ `(abs-declr ,$1 ,$2)))
     (pointer ($$ `(abs-declr ,$1)))
     (direct-abstract-declarator ($$ `(abs-declr ,$1))))

    (direct-abstract-declarator
     ("(" abstract-declarator ")" ($$ `(declr-scope ,$2)))
     (direct-abstract-declarator
      "[" type-qualifier-list assignment-expression "]"
      ($$ `(declr-array ,$1 ,$3 ,$4)))
     (direct-abstract-declarator
      "[" type-qualifier-list "]"
      ($$ `(declr-array ,$1 ,$3)))
     (direct-abstract-declarator
      "[" assignment-expression "]"
      ($$ `(declr-array ,$1 ,$3)))
     (direct-abstract-declarator
      "[" "]" ($$ `(declr-array ,$1)))
     (direct-abstract-declarator
      "[" "static" type-qualifier-list assignment-expression "]"
      ($$ `(declr-array
	    ,$1 ,(tl->list (tl-insert $4 '(stor-spec "static"))) ,$5)))
     (direct-abstract-declarator
      "[" "static" type-qualifier-list "]"
      ($$ `(declr-array ,$1 ,(tl->list (tl-insert $4 '(stor-spec "static"))))))
     (direct-abstract-declarator
      "[" type-qualifier-list "static" assignment-expression "]"
      ($$ `(declr-array
	    ,$1 ,(tl->list (tl-insert $3 '(stor-spec "static"))) ,$5)))
     ;;
     ("[" type-qualifier-list assignment-expression "]"
      ($$ `(declr-anon-array ,$2 ,$3)))
     ("[" type-qualifier-list "]" ($$ `(declr-anon-array ,$2)))
     ("[" assignment-expression "]" ($$ `(declr-anon-array ,$2)))
     ("[" "]" ($$ `(declr-anon-array)))
     ("[" "static" type-qualifier-list assignment-expression "]"
      ($$ `(declr-anon-array
	    ,(tl->list (tl-insert $3 '(stor-spec "static"))) ,$4)))
     ("[" "static" type-qualifier-list "]"
      ($$ `(declr-anon-array ,(tl->list (tl-insert $3 '(stor-spec "static"))))))
     ("[" type-qualifier-list "static" assignment-expression "]"
      ($$ `(declr-anon-array
	    ,(tl->list (tl-insert $2 '(stor-spec "static"))) ,$4)))
     (direct-abstract-declarator "[" "*" "]" ($$ `(declr-star ,$1)))
     ("[" "*" "]" ($$ '(declr-star)))
     (direct-abstract-declarator "(" parameter-type-list ")"
				 ($$ `(abs-ftn-declr ,$1 ,$3)))
     (direct-abstract-declarator "(" ")" ($$ `(abs-ftn-declr ,$1)))
     ("(" parameter-type-list ")" ($$ `(anon-ftn-declr ,$2)))
     ("(" ")" ($$ '(anon-ftn-declr))))

    ;; typedef-name is generated by the lexical analyzer
    (typedef-name ('typename ($$ `(typename ,$1))))

    ;; --------------------------------

    (initializer			; S 6.7.9
     (assignment-expression ($$ `(initzer ,$1)))
     ("{" initializer-list "}" ($$ `(initzer ,(tl->list $2))))
     ("{" initializer-list "," "}" ($$ `(initzer ,(tl->list $2)))))

    ;; The designation productions are from C99.
    (initializer-list
     (designation initializer ($$ (make-tl 'initzer-list $1 $2)))
     (initializer ($$ (make-tl 'initzer-list $1)))
     (initializer-list "," designation initializer ($$ (tl-append $1 $3 $4)))
     (initializer-list "," initializer ($$ (tl-append $1 $3))))

    (designation			; S 6.7.8
     (designator-list "=" ($$ `(desig ,$1))))

    (designator-list
     (designator ($$ (make-tl 'desgr-list $1)))
     (designator-list designator ($$ (tl-append $1 $2))))

    (designator
     ("[" constant-expression "]" ($$ `(array-dsgr ,$2)))
     ("." identifier ($$ `(sel-dsgr ,$2))))

    ;; === statements =========================================================

    (statement
     (labeled-statement)
     (compound-statement)
     (expression-statement)
     (selection-statement)
     (iteration-statement)
     (jump-statement)
     (asm-statement)
     (pragma)
     (cpp-statement))

    (labeled-statement
     (identifier ":" statement ($$ `(labeled-stmt ,$1 ,$3)))
     (identifier ":" attribute-specifier statement
		 ($$ `(labeled-stmt ,$1 ,$4)))
     ("case" constant-expression ":" statement ($$ `(case ,$2 ,$4)))
     ("default" ":" statement ($$ `(default ,$3))))

    (compound-statement
     ("{" ($$ (cpi-push)) block-item-list ($$ (cpi-pop)) "}" 
      ($$ `(compd-stmt ,(tl->list $3))))
     ("{" "}"
      ($$ `(compd-stmt (block-item-list)))))

    (block-item-list
     (block-item ($$ (make-tl 'block-item-list $1)))
     (block-item-list block-item ($$ (tl-append $1 $2))))

    (block-item
     (declaration)
     (statement))
    
    (expression-statement
     (expression ";" ($$ `(expr-stmt ,$1)))
     (";" ($$ '(expr-stmt))))

    (selection-statement
     ("if" "(" expression ")" statement ($prec 'then)
      ($$ `(if ,$3 ,$5)))
     ("if" "(" expression ")" statement "else" statement
      ($$ `(if ,$3 ,$5 ,$7)))
     ("switch" "(" expression ")" statement ($$ `(switch ,$3 ,$5))))

    (iteration-statement
     ("while" "(" expression ")" statement ($$ `(while ,$3 ,$5)))
     ("do" statement "while" "(" expression ")" ";" ($$ `(do-while ,$2 ,$5)))
     ("for" "(" initial-clause opt-expression ";" opt-expression ")" statement
      ($$ `(for ,$3 ,$4 ,$6 ,$8))))
    (initial-clause			; <= added for convenience
     (expression ";")
     (";" ($$ '(expr)))
     (declaration))
    (opt-expression			; <= added for convenience
     ($empty ($$ '(expr)))
     (expression))

    (jump-statement			; S 6.8.6
     ("goto" identifier ";" ($$ `(goto ,$2)))
     ("continue" ";" ($$ '(continue)))
     ("break" ";" ($$ '(break)))
     ("return" expression ";" ($$ `(return ,$2)))
     ("return" ";" ($$ `(return (expr)))))

    (asm-statement
     (asm-expression ";"))
    (asm-expression
     ("__asm__" opt-asm-specifiers "(" string-literal ")"
      ($$ `(asm-expr (@ (extension "GNUC")) ,$4)))
     ("__asm__" opt-asm-specifiers "(" string-literal asm-outputs ")"
      ($$ `(asm-expr (@ (extension "GNUC")) ,$4 ,(tl->list $5))))
     ("__asm__" opt-asm-specifiers "(" string-literal asm-outputs asm-inputs ")"
      ($$ `(asm-expr (@ (extension "GNUC")) ,$4 ,(tl->list $5) ,(tl->list $6))))
     ("__asm__" opt-asm-specifiers "(" string-literal asm-outputs
      asm-inputs asm-clobbers ")"
      ($$ `(asm-expr (@ (extension "GNUC"))
		     ,$4 ,(tl->list $5) ,(tl->list $6) ,(tl->list $7)))))
    (opt-asm-specifiers
     ($empty)
     ("volatile"))
    (asm-outputs
     (":" ($$ (make-tl 'asm-outputs)))
     (":" asm-output ($$ (make-tl 'asm-outputs $2)))
     (asm-outputs "," asm-output ($$ (tl-append $1 $3))))
    (asm-output
     (string-literal "(" identifier ")" ($$ `(asm-operand ,$1 ,$3)))
     ("[" identifier "]" string-literal "(" identifier ")"
      ($$ `(asm-operand ,$2 ,$4 ,$6))))
    (asm-inputs
     (":" ($$ (make-tl 'asm-inputs)))
     (":" asm-input ($$ (make-tl 'asm-inputs $2)))
     (asm-inputs "," asm-input ($$ (tl-append $1 $3))))
    (asm-input
     (string-literal "(" expression ")" ($$ `(asm-operand ,$1 ,$3)))
     ("[" identifier "]" string-literal "(" expression ")"
      ($$ `(asm-operand ,$2 ,$4 ,$6))))
    (asm-clobbers
     (":" ($$ (make-tl 'asm-clobbers)))
     (":" string-literal ($$ (tl-extend (make-tl 'asm-clobbers) $2)))
     (asm-clobbers "," string-literal ($$ (tl-extend $1 (cdr $3)))))

    ;; === top-level forms ====================================================

    (translation-unit			; S 6.9
     (external-declaration-list ($$ (tl->list $1))))

    (external-declaration-list
     ($empty ($$ (make-tl 'trans-unit)))
     (external-declaration-list
      external-declaration
      ;; A ``kludge'' to deal with @code{extern "C" ...}:
      ($$ (if (eqv? (sx-tag $2) 'extern-block)
	      (tl-extend $1 (sx-tail $2 1))
	      (tl-append $1 $2)))))

    (external-declaration		; S 6.9
     (function-definition)
     (declaration)
     (lone-comment)
     (cpp-statement)
     (pragma)
     ("extern" $string "{"
      ($$ (cpi-dec-blev!)) external-declaration-list ($$ (cpi-inc-blev!)) "}"
      ($$ `(extern-block
	    (extern-begin ,$2) ,@(sx-tail (tl->list $5) 1) (extern-end))))
     (";" ($$ `(decl (@ (extension "GNUC"))))))
    
    (function-definition
     (declaration-specifiers
      declarator compound-statement
      ($$ `(fctn-defn ,$1 ,$2 ,$3)))
     ;; K&R function definitions are not compatible with attribute-specifiers.
     ;;(declaration-specifiers
     ;; declarator declaration-list compound-statement
     ;; ($$ `(knr-fctn-defn ,$1 ,$2 ,$3 ,$4)))
     )

    ;; K&R function-definition parameter list
    ;;(declaration-list (declaration-list-1 ($$ (tl->list $1))))
    ;;(declaration-list-1
    ;; (declaration ($$ (make-tl 'decl-list $1)))
    ;; (declaration-list-1 declaration ($$ (tl-append $1 $2))))

    ;; non-terminal leaves
    (identifier ($ident ($$ `(ident ,$1))))
    (constant
     ($fixed ($$ `(fixed ,$1)))		; integer literal
     ($float ($$ `(float ,$1)))		; floating literal
     ($chlit ($$ `(char ,$1)))		; char literal
     ($chlit/L ($$ `(char (@ (type "wchar_t")) ,$1)))
     ($chlit/u ($$ `(char (@ (type "char16_t")) ,$1)))
     ($chlit/U ($$ `(char (@ (type "char32_t")) ,$1))))
    (string-literal (string-literal-1 ($$ (tl->list $1))))
    (string-literal-1
     ($string ($$ (make-tl 'string $1))) ; string-constant
     (string-literal-1 $string ($$ (tl-append $1 $2))))
    (code-comment ($code-comm ($$ `(comment ,$1))))
    (lone-comment ($lone-comm ($$ `(comment ,$1))))
    (cpp-statement ('cpp-stmt ($$ `(cpp-stmt ,$1))))
    (pragma
     ($pragma ($$ `(pragma ,$1)))
     ("_Pragma" "(" string-literal ")" ($$ `(pragma ,$3))))

    )))

;;; === parsers =========================

;; We setup dev parser because circular dependence between lexer and parser
;; due to parsing include files as units for code and decl mode.
;; update: This is doable now (see parser.scm) but wait until it's needed.

(define c99-mach
  (compact-machine
   (hashify-machine
    (make-lalr-machine c99-spec))
   #:keep 2
   #:keepers '($code-comm $lone-comm $pragma)))

(define c99x-spec (restart-spec c99-spec 'expression))

(define c99x-mach
  (compact-machine
   (hashify-machine
    (make-lalr-machine c99x-spec))
   #:keep 2
   #:keepers '($code-comm $lone-comm $pragma)))

;;; =====================================

;; @deffn {Procedure} gen-c99-files [dir] => #t
;; Update or generate the files @quot{c99act.scm} and @quot{c99tab.scm}.
;; These are the tables and actions for the C99 parser.
;; If there are no changes to existing files, no update occurs.
;; @end deffn
(define* (gen-c99-files #:optional (path "."))
  (define (mdir file) (mach-dir path file))
  (write-lalr-actions c99-mach (mdir "c99-act.scm.new") #:prefix "c99-")
  (write-lalr-tables c99-mach (mdir "c99-tab.scm.new") #:prefix "c99-")
  (write-lalr-actions c99x-mach (mdir "c99x-act.scm.new") #:prefix "c99x-")
  (write-lalr-tables c99x-mach (mdir "c99x-tab.scm.new") #:prefix "c99x-")
  (let ((a (move-if-changed (mdir "c99-act.scm.new") (mdir "c99-act.scm")))
	(b (move-if-changed (mdir "c99-tab.scm.new") (mdir "c99-tab.scm")))
	(c (move-if-changed (mdir "c99x-act.scm.new") (mdir "c99x-act.scm")))
	(d (move-if-changed (mdir "c99x-tab.scm.new") (mdir "c99x-tab.scm"))))
    (or a b c d)))

;; --- last line ---

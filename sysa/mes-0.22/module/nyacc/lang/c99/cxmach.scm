;;; nyacc/lang/c99/cxmach.scm - constant expression grammar

;; Copyright (C) 2018 Matthew R. Wette
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

;;; Code:

(define-module (nyacc lang c99 cxmach)
  #:export (c99cx-spec c99cx-mach gen-c99cx-files)
  #:use-module (nyacc lalr)
  #:use-module (nyacc parse)
  #:use-module (nyacc lex)
  #:use-module (nyacc util)
  #:use-module (nyacc lang util)
  #:use-module (rnrs arithmetic bitwise)
  #:use-module ((srfi srfi-43) #:select (vector-map vector-for-each))
  #:use-module (system foreign))

(define c99cx-spec
  (lalr-spec
   (notice (string-append "Copyright (C) 2018 Matthew R. Wette" license-lgpl3+))
   (expect 0)
   (start constant-expression)
   (grammar
    (primary-expression
     (identifier ($$ `(p-expr ,$1)))
     (constant ($$ `(p-expr ,$1)))
     (string-literal ($$ `(p-expr ,(tl->list $1))))
     ("(" constant-expression ")" ($$ $2)))
    (postfix-expression
     (primary-expression)
     (postfix-expression "[" constant-expression "]" ($$ `(array-ref ,$3 ,$1)))
     (postfix-expression "." identifier ($$ `(d-sel ,$3 ,$1)))
     (postfix-expression "->" identifier ($$ `(i-sel ,$3 ,$1)))
     (postfix-expression "++" ($$ `(post-inc ,$1)))
     (postfix-expression "--" ($$ `(post-dec ,$1))))
    (unary-expression
     (postfix-expression)		; S 6.5.3
     ("++" unary-expression ($$ `(pre-inc ,$2)))
     ("--" unary-expression ($$ `(pre-dec ,$2)))
     (unary-operator cast-expression ($$ (list $1 $2)))
     ("sizeof" unary-expression ($$ `(sizeof-expr ,$2)))
     ;;("sizeof" "(" type-name ")" ($$ `(sizeof-type ,$3)))
     )
    (unary-operator ("&" ($$ 'ref-to)) ("*" ($$ 'de-ref))
		    ("+" ($$ 'pos)) ("-" ($$ 'neg))
		    ("~" ($$ 'bitwise-not)) ("!" ($$ 'not)))
    (cast-expression
     (unary-expression)
     ;;("(" type-name ")" cast-expression ($$ `(cast ,$2 ,$4)))
     )
    (multiplicative-expression
     (cast-expression)
     (multiplicative-expression "*" cast-expression ($$ `(mul ,$1 ,$3)))
     (multiplicative-expression "/" cast-expression ($$ `(div ,$1 ,$3)))
     (multiplicative-expression "%" cast-expression ($$ `(mod ,$1 ,$3))))
    (additive-expression
     (multiplicative-expression)
     (additive-expression "+" multiplicative-expression ($$ `(add ,$1 ,$3)))
     (additive-expression "-" multiplicative-expression ($$ `(sub ,$1 ,$3))))
    (shift-expression
     (additive-expression)
     (shift-expression "<<" additive-expression ($$ `(lshift ,$1 ,$3)))
     (shift-expression ">>" additive-expression ($$ `(rshift ,$1 ,$3))))
    (relational-expression
     (shift-expression)
     (relational-expression "<" shift-expression ($$ `(lt ,$1 ,$3)))
     (relational-expression ">" shift-expression ($$ `(gt ,$1 ,$3)))
     (relational-expression "<=" shift-expression ($$ `(le ,$1 ,$3)))
     (relational-expression ">=" shift-expression ($$ `(ge ,$1 ,$3))))
    (equality-expression
     (relational-expression)
     (equality-expression "==" relational-expression ($$ `(eq ,$1 ,$3)))
     (equality-expression "!=" relational-expression ($$ `(ne ,$1 ,$3))))
    (bitwise-and-expression
     (equality-expression)
     (bitwise-and-expression
      "&" equality-expression ($$ `(bitwise-and ,$1 ,$3))))
    (bitwise-xor-expression
     (bitwise-and-expression)
     (bitwise-xor-expression
      "^" bitwise-and-expression ($$ `(bitwise-xor ,$1 ,$3))))
    (bitwise-or-expression
     (bitwise-xor-expression)
     (bitwise-or-expression
      "|" bitwise-xor-expression ($$ `(bitwise-or ,$1 ,$3))))
    (logical-and-expression
     (bitwise-or-expression)
     (logical-and-expression
      "&&" bitwise-or-expression ($$ `(and ,$1 ,$3))))
    (logical-or-expression
     (logical-and-expression)
     (logical-or-expression
      "||" logical-and-expression ($$ `(or ,$1 ,$3))))
    (conditional-expression
     (logical-or-expression)
     (logical-or-expression
      "?" constant-expression
      ":" conditional-expression ($$ `(cond-expr ,$1 ,$3 ,$5))))
    (constant-expression
     (conditional-expression))
    ;;
    (identifier
     ($ident ($$ `(ident ,$1))))
    (constant
     ($fixed ($$ `(fixed ,$1)))		; integer literal
     ($float ($$ `(float ,$1)))		; floating literal
     ($chlit ($$ `(char ,$1)))		; char literal
     ($chlit/L ($$ `(char (@ (type "wchar_t")) ,$1)))
     ($chlit/u ($$ `(char (@ (type "char16_t")) ,$1)))
     ($chlit/U ($$ `(char (@ (type "char32_t")) ,$1))))
    (string-literal
     ($string ($$ (make-tl 'string $1))) ; string-constant
     (string-literal $string ($$ (tl-append $1 $2)))))))

(define c99cx-mach
  (compact-machine
   (hashify-machine
    (make-lalr-machine c99cx-spec))))

;;; =====================================

;; @item gen-c99cx-files [dir] => #t
;; Update or generate the files @quot{cppact.scm} and @quot{cpptab.scm}.
;; If there are no changes to existing files, no update occurs.
(define* (gen-c99cx-files #:optional (path "."))
  (define (mdir file) (mach-dir path file))
  (write-lalr-actions c99cx-mach (mdir "c99cx-act.scm.new") #:prefix "c99cx-")
  (write-lalr-tables c99cx-mach (mdir "c99cx-tab.scm.new") #:prefix "c99cx-")
  (let ((a (move-if-changed (mdir "c99cx-act.scm.new") (mdir "c99cx-act.scm")))
	(b (move-if-changed (mdir "c99cx-tab.scm.new") (mdir "c99cx-tab.scm"))))
    (or a b)))

;; --- last line ---

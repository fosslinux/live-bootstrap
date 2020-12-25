;;; nyacc/lang/c99/c99eval.scm - evaluate constant expressions

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

(define-module (nyacc lang c99 cxeval)
  #:export (parse-c99-cx eval-c99-cx)
  #:use-module (nyacc lalr)
  #:use-module (nyacc parse)
  #:use-module (nyacc lex)
  #:use-module (nyacc util)
  #:use-module ((nyacc lang util) #:select (make-tl tl-append tl->list))
  #:use-module (nyacc lang sx-util)
  #:use-module (nyacc lang c99 cpp)
  #:use-module (nyacc lang c99 munge)
  #:use-module (rnrs arithmetic bitwise)
  #:use-module ((srfi srfi-43) #:select (vector-map vector-for-each))
  #:use-module (system foreign))

(use-modules (ice-9 pretty-print))
(define pp pretty-print)
(define (sf fmt . args) (apply simple-format #t fmt args))

(define ffi-type-map
  `(("void" . ,void) ("float" . ,float) ("double" . ,double) ("short" . ,short)
    ("short int" . ,short) ("signed short" . ,short)
    ("signed short int" . ,short) ("int" . ,int) ("signed" . ,int)
    ("signed int" . ,int) ("long" . ,long) ("long int" . ,long)
    ("signed long" . ,long) ("signed long int" . ,long)
    ("unsigned short int" . ,unsigned-short)
    ("unsigned short" . ,unsigned-short)
    ("unsigned int" . ,unsigned-int) ("unsigned" . ,unsigned-int)
    ("unsigned long int" . ,unsigned-long) ("unsigned long" . ,unsigned-long)
    ("char" . ,int8) ("signed char" . ,int8) ("unsigned char" . ,uint8)
    ("wchar_t" . ,int) ("char16_t" . ,int16) ("char32_t" . ,int32)
    ("long long" . ,long) ("long long int" . ,long)
    ("signed long long" . ,long) ("signed long long int" . ,long)
    ("unsigned long long" . ,unsigned-long)
    ("unsigned long long int" . ,unsigned-long) ("_Bool" . ,int8)
    ("size_t" . ,size_t)))

(define (sizeof-type name)
  (or (and=> (assoc-ref ffi-type-map name) sizeof)
      (throw 'nyacc-error "bad type")))

;; (string "abc" "dev")
(define (sizeof-string-const value)
  #f)

(include-from-path "nyacc/lang/c99/mach.d/c99cx-act.scm")
(include-from-path "nyacc/lang/c99/mach.d/c99cx-tab.scm")

(define c99cx-raw-parser
  (make-lalr-parser
   (acons 'act-v c99cx-act-v c99cx-tables)))

(define gen-c99cx-lexer
  (let* ((reader (make-comm-reader '(("/*" . "*/"))))
	 (comm-skipper (lambda (ch) (reader ch #f))))
    (make-lexer-generator c99cx-mtab
			  #:comm-skipper comm-skipper
			  #:chlit-reader read-c-chlit
			  #:num-reader read-c-num)))

(define (parse-c99cx text)
  (with-throw-handler
   'nyacc-error
   (lambda ()
     (with-input-from-string text
       (lambda () (c99cx-raw-parser (gen-c99cx-lexer)))))
   (lambda (key fmt . args)
     (apply throw 'cpp-error fmt args))))

(define (expand-typename typename udict)
  (let* ((decl `(udecl (decl-spec-list
			(type-spec (typename ,typename)))
		       (declr (ident "_"))))
	 (xdecl (expand-typerefs decl udict))
	 (xname (and xdecl (sx-ref* xdecl 1 1 1 1))))
    xname))

;; (sizeof type-name)
;; (type-name specificer-qualifier-list abstract-declarator)
;; (decl-spec-list 
;; (abs-decl
(define (eval-sizeof-type tree udict)
  (sx-match (sx-ref tree 1)
    ((type-name (decl-spec-list (type-spec (typename ,name))))
     (let* ((xname (expand-typename name udict))
	    (ffi-type (assoc-ref ffi-type-map xname)))
       (unless ffi-type
	 (sf "need to expand ~S\n" name) (pp tree)
	 (error "eval-sizeof-type: missed typedef (work to go)"))
       (sizeof ffi-type)))
    ((type-name (decl-spec-list (type-spec (fixed-type ,name))))
     (let* ((ffi-type (assoc-ref ffi-type-map name)))
       (sizeof ffi-type)))
    ((type-name (decl-spec-list (type-spec (float-type ,name))))
     (let* ((ffi-type (assoc-ref ffi-type-map name)))
       (sizeof ffi-type)))
    ((type-name (decl-spec-list (type-spec . ,_1)) (abs-declr (pointer)))
     (sizeof '*))
    (else
     (sf "eval-sizeof-type missed:\n") (pp (sx-ref tree 1))
     (error "can't eval sizeof(type)"))))
  
;; (sizeof unary-expr)
;;    (primary-expression			; S 6.5.1
;;     (identifier ($$ `(p-expr ,$1)))
;;     (constant ($$ `(p-expr ,$1)))
;;     (string-literal ($$ `(p-expr ,(tl->list $1))))
;;     ("(" expression ")" ($$ $2))
;;     ("(" "{" block-item-list "}" ")"
;;      ($$ `(stmt-expr (@ (extension "GNUC")) ,$3)))
;;     )
(define (eval-sizeof-expr tree udict)
  (let* ((expr (sx-ref tree 1)))
    (sx-match expr
      ((p-expr (string . ,strl))
       (let loop ((l 0) (sl strl))
	 (if (pair? sl) (loop (+ l (string-length (car sl))) (cdr sl)) l)))
      (else #f))))

(define (eval-ident name udict ddict)
  (cond
   ((assoc-ref ddict name) =>
    (lambda (hit)
      ;; This should actually go through the cpp-expander first methinks.
      (and (string? hit)
	   (let ((expr (parse-cpp-expr hit)))
	     (eval-c99-cx expr udict ddict)))))
   (else
    ;;(error "missed" name)
    #f)))

;; @deffn {Procedure} eval-c99-cx tree [udict [ddict]]
;; Evaluate the constant expression or return #f
;; @end deffn
(define* (eval-c99-cx tree #:optional udict ddict)
  (define (fail) #f)
  (letrec
      ((ev (lambda (ex ix) (eval-expr (sx-ref ex ix))))
       (ev1 (lambda (ex) (ev ex 1)))	; eval expr in arg 1
       (ev2 (lambda (ex) (ev ex 2)))	; eval expr in arg 2
       (ev3 (lambda (ex) (ev ex 3)))	; eval expr in arg 3
       (uop (lambda (op ex) (and op ex (op ex))))
       (bop (lambda (op lt rt) (and op lt rt (op lt rt))))
       (eval-expr
	(lambda (tree)
	  (case (car tree)
	    ((fixed) (string->number (cnumstr->scm (sx-ref tree 1))))
	    ((float) (string->number (cnumstr->scm (sx-ref tree 1))))
	    ((char) (char->integer (string-ref (sx-ref tree 1) 0)))
	    ((string) (string-join (sx-tail tree 1) ""))
	    ((pre-inc post-inc) (uop 1+ (ev1 tree)))
	    ((pre-dec post-dec) (uop 1- (ev1 tree)))
	    ((pos) (and tree (ev1 tree)))
	    ((neg) (uop - (ev1 tree)))
	    ((not) (and tree (if (equal? 0 (ev1 tree)) 1 0)))
	    ((mul) (bop * (ev1 tree) (ev2 tree)))
	    ((div) (bop / (ev1 tree) (ev2 tree)))
	    ((mod) (bop modulo (ev1 tree) (ev2 tree)))
	    ((add) (bop + (ev1 tree) (ev2 tree)))
	    ((sub) (bop - (ev1 tree) (ev2 tree)))
	    ((lshift) (bop bitwise-arithmetic-shift-left (ev1 tree) (ev2 tree)))
	    ((rshift) (bop bitwise-arithmetic-shift-right (ev1 tree) (ev2 tree)))
	    ((lt) (if (bop < (ev1 tree) (ev2 tree)) 1 0))
	    ((le) (if (bop <= (ev1 tree) (ev2 tree)) 1 0))
	    ((gt) (if (bop > (ev1 tree) (ev2 tree)) 1 0))
	    ((ge) (if (bop >= (ev1 tree) (ev2 tree)) 1 0))
	    ((eq) (if (bop = (ev1 tree) (ev2 tree)) 1 0))
	    ((ne) (if (bop = (ev1 tree) (ev2 tree)) 0 1))
	    ((bitwise-not) (uop lognot (ev1 tree)))
	    ((bitwise-or) (bop logior (ev1 tree) (ev2 tree)))
	    ((bitwise-xor) (bop logxor (ev1 tree) (ev2 tree)))
	    ((bitwise-and) (bop logand (ev1 tree) (ev2 tree)))
	    ;;
	    ((or)
	     (let ((e1 (ev1 tree)) (e2 (ev2 tree)))
	       (if (and e1 e2) (if (and (zero? e1) (zero? e2)) 0 1) #f)))
	    ((and)
	     (let ((e1 (ev1 tree)) (e2 (ev2 tree)))
	       (if (and e1 e2) (if (or (zero? e1) (zero? e2)) 0 1) #f)))
	    ((cond-expr)
	     (let ((e1 (ev1 tree)) (e2 (ev2 tree)) (e3 (ev3 tree)))
	       (if (and e1 e2 e3) (if (zero? e1) e3 e2) #f)))
	    ;;
	    ((sizeof-type) (eval-sizeof-type tree udict))
	    ((sizeof-expr) (eval-sizeof-expr tree udict))
	    ((ident) (eval-ident (sx-ref tree 1) udict ddict))
	    ((p-expr) (ev1 tree))
	    ((cast) (ev2 tree))
	    ((fctn-call) #f)		; assume not constant
	    ;;
	    ;; TODO 
	    ((comp-lit) (fail))		; return a bytearray
	    ((comma-expr) (fail))
	    ((i-sel) (fail))
	    ((d-sel) (fail))
	    ((array-ref) (fail))
	    ;; 
	    (else (fail))))))
    (eval-expr tree)))

;; --- last line ---

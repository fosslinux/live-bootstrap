;;; nyacc/lang/c99/util.scm - C parser utilities

;; Copyright (C) 2015-2019 Matthew R. Wette
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

(define-module (nyacc lang c99 util)
  #:export (c99-def-help
	    c99-std-help
	    get-gcc-cpp-defs
	    get-gcc-inc-dirs 
	    remove-inc-trees
	    merge-inc-trees!
	    move-attributes attrl->attrs attrs->attrl extract-attr
	    elifify)
  #:use-module (nyacc lang util)
  #:use-module (nyacc lang sx-util)
  #:use-module ((srfi srfi-1) #:select (append-reverse fold-right))
  #:use-module (srfi srfi-2)		; and-let*
  #:use-module (sxml fold)
  #:use-module (ice-9 popen)		; gen-gcc-cpp-defs
  #:use-module (ice-9 rdelim)		; gen-gcc-cpp-defs
  )

(define c99-def-help
  '(("__builtin"
     "__builtin_va_list=void*"
     "__inline__=inline" "__inline=__inline__"
     "__restrict__=restrict" "__restrict=__restrict__"
     "__signed__=signed" "__signed=__signed__"
     "asm(X)=__asm__(X)" "__asm(X)=__asm__(X)"
     "__attribute(X)=__attribute__(X)"
     "__volatile__=volatile" "__volatile=__volatile__"
     "__extension__=" "__extension=__extension__"
     "asm=__asm__" "__asm=__asm__"
     "__attribute(X)=__attribute__(X)"
     )))

;; include-helper for C99 std
(define c99-std-help
  (append
   c99-def-help
   '(("alloca.h")
     ("complex.h" "complex" "imaginary" "_Imaginary_I=C99_ANY" "I=C99_ANY")
     ("ctype.h")
     ("fenv.h" "fenv_t" "fexcept_t")
     ("float.h" "float_t" "FLT_MAX=C99_ANY" "DBL_MAX=C99_ANY")
     ("inttypes.h"
      "int8_t" "uint8_t" "int16_t" "uint16_t" "int32_t" "uint32_t"
      "int64_t" "uint64_t" "uintptr_t" "intptr_t" "intmax_t" "uintmax_t"
      "int_least8_t" "uint_least8_t" "int_least16_t" "uint_least16_t"
      "int_least32_t" "uint_least32_t" "int_least64_t" "uint_least64_t"
      "imaxdiv_t")
     ("limits.h"
      "INT_MIN=C99_ANY" "INT_MAX=C99_ANY" "LONG_MIN=C99_ANY" "LONG_MAX=C99_ANY")
     ("math.h" "float_t" "double_t")
     ("regex.h" "regex_t" "regmatch_t")
     ("setjmp.h" "jmp_buf")
     ("signal.h" "sig_atomic_t")
     ("stdarg.h" "va_list")
     ("stddef.h" "ptrdiff_t" "size_t" "wchar_t")
     ("stdint.h"
      "int8_t" "uint8_t" "int16_t" "uint16_t" "int32_t" "uint32_t"
      "int64_t" "uint64_t" "uintptr_t" "intptr_t" "intmax_t" "uintmax_t"
      "int_least8_t" "uint_least8_t" "int_least16_t" "uint_least16_t"
      "int_least32_t" "uint_least32_t" "int_least64_t" "uint_least64_t")
     ("stdio.h" "FILE" "size_t")
     ("stdlib.h" "div_t" "ldiv_t" "lldiv_t" "wchar_t")
     ("string.h" "size_t")
     ("strings.h" "size_t")
     ("time.h" "time_t" "clock_t" "size_t")
     ("unistd.h" "size_t" "ssize_t" "div_t" "ldiv_t")
     ("wchar.h" "wchar_t" "wint_t" "mbstate_t" "size_t")
     ("wctype.h" "wctrans_t" "wctype_t" "wint_t"))))

(define (resolve-CC CC)
  (cond
   (CC CC)
   ((getenv "CC") => identity)
   (else "gcc")))

;; @deffn {Procedure} convert-def line
;; Convert string in gcc cpp defs to pair of strings for term and replacement.
;; @end deffn
(define (convert-line line)
  (with-input-from-string line
    (lambda ()
      (let loop ((term '()) (acc '()) (st 0) (ch (read-char)))
	(case st
	  ((0) ;; skip #define
	   (if (char=? ch #\space)
	       (loop term acc 1 (read-char))
	       (loop term acc 0 (read-char))))
	  ((1) ;; read term
	   (if (char=? ch #\space)
	       (loop (reverse-list->string acc) '() 2 (read-char))
	       (loop term (cons ch acc) st (read-char))))
	  ((2) ;; read rest
	   (if (or (eof-object? ch) (char=? ch #\newline))
	       (string-append term "=" (reverse-list->string acc))
	       (loop term (cons ch acc) st (read-char)))))))))

;; @deffn {Procedure} get-gcc-cpp-defs [args] [#:CC "gcc"] => '("ABC=123" ...)
;; Generate a list of default defines produced by gcc (or other comiler).
;; If keyword arg @arg{CC} is not provided this procedure looks for environment
;; variable @code{"CC"}, else it defaults to @code{"gcc"}.
;; @end deffn
(define* (get-gcc-cpp-defs #:optional (args '()) #:key CC)
  ;; @code{"gcc -dM -E"} will generate lines like @code{"#define ABC 123"}.
  ;; We generate and return a list like @code{'(("ABC" . "123") ...)}.
  (let* ((cmd (string-append (resolve-CC CC) " -dM -E - </dev/null"))
	 (ip (open-input-pipe cmd)))
    (let loop ((line (read-line ip 'trim)))
      (if (eof-object? line) '()
	  (cons (convert-line line) (loop (read-line ip 'trim)))))))

;; @deffn {Procedure} get-gcc-inc-dirs [args] [#:CC "gcc"] =>
;; Generate a list of compiler-internal include directories (for gcc).  If
;; keyword arg @arg{CC} is not provided this procedure looks for environment
;; variable @code{"CC"}, else it defaults to @code{"gcc"}.
;; @end deffn
(define* (get-gcc-inc-dirs #:optional (args '()) #:key CC)
  (let ((ip (open-input-pipe (string-append
			      (resolve-CC CC) " -E -Wp,-v - </dev/null 2>&1"))))
    (let loop ((dirs '()) (grab #f) (line (read-line ip 'trim)))
      (cond
       ((eof-object? line) dirs)
       ((string=? line "#include <...> search starts here:")
	(loop dirs #t (read-line ip 'trim)))
       ((string=? line "End of search list.") dirs)
       (grab
	(loop (cons (string-trim-both line) dirs)
	      grab (read-line ip 'trim)))
       (else
	(loop dirs grab (read-line ip 'trim)))))))

;; @deffn {Procedure} remove-inc-trees tree
;; Remove the trees included with cpp-include statements.
;; @example
;; '(... (cpp-stmt (include "<foo.h>" (trans-unit ...))) ...)
;; => '(... (cpp-stmt (include "<foo.h>")) ...)
;; @end example
;; @end deffn
(define (remove-inc-trees tree)
  (if (not (eqv? 'trans-unit (car tree)))
      (throw 'nyacc-error "expecting c-tree"))
  (let loop ((rslt (make-tl 'trans-unit))
	     ;;(head '(trans-unit)) (tail (cdr tree))
	     (tree (cdr tree)))
    (cond
     ((null? tree) (tl->list rslt))
     ((and (eqv? 'cpp-stmt (car (car tree)))
	   (eqv? 'include (caadr (car tree))))
      (loop (tl-append rslt `(cpp-stmt (include ,(cadadr (car tree)))))
	    (cdr tree)))
     (else (loop (tl-append rslt (car tree)) (cdr tree))))))

;; @deffn {Procedure} merge-inc-trees! tree => tree
;; This will (recursively) merge code from cpp-includes into the tree.
;; @example
;; (trans-unit
;;  (decl (a))
;;  (cpp-stmt (include "<hello.h>" (trans-unit (decl (b)))))
;;  (decl (c)))
;; =>
;; (trans-unit (decl (a)) (decl (b)) (decl (c)))
;; @end example
;; @end deffn
(define (merge-inc-trees! tree)

  ;; @item find-span (trans-unit a b c) => ((a . +->) . (c . '())
  (define (find-span tree)
    (cond
     ((not (pair? tree)) '())		; maybe parse failed
     ((not (eqv? 'trans-unit (car tree))) (throw 'c99-error "expecting c-tree"))
     ((null? (cdr tree)) (throw 'c99-error "null c99-tree"))
     (else
      (let ((fp tree))			; first pair
	(let loop ((lp tree)		; last pair
		   (np (cdr tree)))	; next pair
	  (cond
	   ((null? np) (cons (cdr fp) lp))
	   ;; The following is an ugly hack to find cpp-include
	   ;; with trans-unit attached.
	   ((and-let* ((expr (car np))
		       ((eqv? 'cpp-stmt (car expr)))
		       ((eqv? 'include (caadr expr)))
		       (rest (cddadr expr))
		       ((pair? rest))
		       (span (find-span (car rest))))
		      (set-cdr! lp (car span))
		      (loop (cdr span) (cdr np))))
	   (else
	    (set-cdr! lp np)
	    (loop np (cdr np)))))))))

  ;; Use cons to generate a new reference:
  ;; (cons (car tree) (car (find-span tree)))
  ;; or not:
  (find-span tree)
  tree)


;; --- attributes ----------------------

(define (join-string-literal str-lit)
  (sx-list 'string (sx-attr str-lit) (string-join (sx-tail str-lit) "")))

;; used in c99-spec actions for attribute-specifiers
(define (attr-expr-list->string attr-expr-list)
  (string-append "(" (string-join (cdr attr-expr-list) ",") ")"))

;; ((attribute-list ...) (type-spec ...) (attribute-list ...)) =>
;;   (values (attribute-list ...)  ((type-spec ...) ...))

;; @deffn extract-attr tail => (values attr-tree tail)
;; Extract attributes from a sexp tail.
;; @end deffn
(define (extract-attr tail) ;; => (values attr-tree tail)
  (let loop ((atl '()) (tail1 '()) (tail0 tail))
    (cond
     ((null? tail0)
      (if (null? atl)
	  (values '() tail)
	  (values `(attribute-list . ,atl) (reverse tail1))))
     ((eq? 'attribute-list (sx-tag (car tail0)))
      (loop (append (sx-tail (car tail0)) atl) tail1 (cdr tail0)))
     (else
      (loop atl (cons (car tail0) tail1) (cdr tail0))))))

;; (attribute-list (attribute (ident "__packed__")) ...)
;;  =>
;; (attributes "__packed__;...")
;; OR
;; () => ()
(define (attrl->attrs attr-list)
  (define (spec->str spec)
    (sx-match spec
      ((ident ,name) name)
      ((attribute ,name) (spec->str name))
      ((attribute ,name ,args)
       (string-append (spec->str name) "(" (spec->str args) ")"))
      ((attr-expr-list . ,expr-list)
       (string-join (map spec->str expr-list) ","))
      ((fixed ,val) val)
      ((float ,val) val)
      ((char ,val) val)
      ((string . ,val) (string-append "\"" (string-join val "") "\""))
      ((type-name (decl-spec-list (type-spec ,spec))) (spec->str spec))
      ((fixed-type ,name) name)
      ((float-type ,name) name)
      (,_ (sferr "c99/util: missed ~S\n" spec) "MISSED")))
  (if (null? attr-list) '()
      `(attributes ,(string-join (map spec->str (sx-tail attr-list)) ";"))))

;; (attributes "__packed__;__aligned__;__alignof__(8)")
;;   =>
;; (attribute-list (attribute "__packed
;; OR
;; #f => #f
(use-modules (nyacc lex))

(define (astng->atree form)
  (define a-mtab
    '(("(" . lparen) ((")" . rparen))
      ("," . comma) ($ident . ident)))
  (define attlexgen (make-lexer-generator a-mtab))
  (define attlex (attlexgen))

  (with-input-from-string form
    (lambda ()
      (define (p-expr-list lx) ;; see 'lparen
	(and
	 (eq? 'lparen (car lx))
	 (let loop ((args '()) (lx (attlex)))
	   (case (car lx)
	     ((rparen) `(attr-expr-list . ,args))
	     ((comma) (loop args (attlex)))
	     (else (p-expr lx))))))
      (define (p-expr lx)
	#f)
      (let ((lx (attlex)))
	(sferr "lx=~S\n" lx)
	(case (car lx)
	  ((ident)
	   (let* ((id (cdr lx)) (lx (attlex)))
	     (case (car lx)
	       (($end) `(attribute ,id))
	       ((lparen) `(attribute ,id ,(p-expr-list lx)))
	       (else (throw 'nyacc-error "error ~S" lx)))))
	  (else (throw 'nyacc-error "missed ~S" lx)))))))
		
(define (attrs->attrl attr-sexp)
  (and
   attr-sexp 
   (let* ((attrs (cadr attr-sexp))
	  (attl (string-split attrs #\;)))
     `(attribute-list ,@(map astng->atree attl)))))

;; @deffn {Procedure} move-attributes sexp
;; Given a sexpr, combine attribute-list kids and move to attribute ??
;; @example
;; (decl (decl-spec-list
;;         (attributes "__packed__" "__aligned__")
;;         (attributes "__alignof__(8)"))
;;         (type-spec (fixed-type "int")))
;;       (declr-init-list ...))
;;  =>
;; (decl (decl-spec-list
;;         (@ (attributes "__packed__;__aligned__;__alignof__(8)"))
;;         (type-spec (fixed-type "int")))
;;       (declr-init-list ...))
;; @end example
;; @end deffn
(define (move-attributes sexp)
  (let ((tag (sx-tag sexp)) (attr (sx-attr sexp)) (tail (sx-tail sexp)))
    (call-with-values (lambda () (extract-attr tail))
      (lambda (attrl stail)
	(sx-cons*
	 tag 
	 (cond
	  ((null? attrl) attr)
	  ((null? attr)`(@ ,(attrl->attrs attrl)))
	  (else (append attr (list (attrl->attrs attrl)))))
	 stail)))))

;; --- random stuff 

;; @deffn {Procedure} elifify tree => tree
;; This procedure will find patterns of
;; @example
;; (if cond-1 then-part-1
;;            (if cond-2 then-part-2
;;                       else-part-2
;; @end example
;; @noindent
;; and convert to
;; @example
;; (if cond-1 then-part-1
;;            (elif cond-2 then-part-2)
;;            else-part-2
;; @end example
;; @end deffn
(define (elifify tree)
  (define (fU tree)
    (sx-match tree
      ((if ,x1 ,t1 (if ,x2 ,t2 (else-if ,x3 ,t3) . ,rest))
       `(if ,x1 ,t1 (else-if ,x2 ,t2) (else-if ,x3 ,t3) . ,rest))
      ((if ,x1 ,t1 (if ,x2 ,t2 . ,rest))
       `(if ,x1 ,t1 (else-if ,x2 ,t2) . ,rest))
      (else
       tree)))
  (foldt fU identity tree))

;; --- last line ---

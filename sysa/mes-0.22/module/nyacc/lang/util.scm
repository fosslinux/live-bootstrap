;;; module/nyacc/util.scm - runtime utilities for the parsers

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

;;; Code:

(define-module (nyacc lang util)
  #:export (license-lgpl3+
	    report-error
	    *input-stack* push-input pop-input
	    reset-input-stack input-stack-portinfo
	    make-tl tl->list ;; rename?? to tl->sx for sxml-expr
	    tl-append tl-insert tl-extend tl+attr tl+attr*
	    ;; for pretty-printing
	    make-protect-expr make-pp-formatter make-pp-formatter/ugly
	    ;; for ???
	    move-if-changed
	    cintstr->scm
	    sferr pperr
	    mach-dir
	    ;; deprecated
	    lang-crn-lic)
  #:use-module ((srfi srfi-1) #:select (find fold fold-right))
  #:use-module (ice-9 pretty-print))
(cond-expand
  (mes)
  (guile-2)
  (guile-3)
  (guile
   (use-modules (ice-9 optargs))
   (use-modules (srfi srfi-16)))
  (else))

;; This is a generic copyright/licence that will be printed in the output
;; of the examples/nyacc/lang/*/ actions.scm and tables.scm files.
(define license-lgpl3+ "

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.
See the file COPYING included with the this distribution.")
(define lang-crn-lic license-lgpl3+)

(define (sferr fmt . args)
  (apply simple-format (current-error-port) fmt args))
(define (pperr exp . kw-args)
  (apply pretty-print exp (current-error-port) kw-args))

;; @deffn {Procedure} report-error fmt args
;; Report an error, to stderr, providing file and line num info, and add
;; newline.  This also reports context of parent files.
;; @end deffn
(define (report-error fmt args)
  (let ((fn (or (port-filename (current-input-port)) "(unknown)"))
	(ln (1+ (port-line (current-input-port)))))
    (apply simple-format (current-error-port)
	   (string-append "~A:~A: " fmt "\n") fn ln args)
    (for-each
     (lambda (pair)
       (simple-format (current-error-port) "  at ~A:~A\n" (car pair) (cdr pair)))
     (input-stack-portinfo))))

;; === input stack =====================

(define *input-stack* (make-fluid))

(define (reset-input-stack)
  (fluid-set! *input-stack* '()))

(define (push-input port)
  (let ((curr (current-input-port))
	(ipstk (fluid-ref *input-stack*)))
    (fluid-set! *input-stack* (cons curr ipstk))
    ;;(sferr "~S pu=>\n" (length ipstk))
    (set-current-input-port port)))

;; Return #f if empty
(define (pop-input)
  (let ((ipstk (fluid-ref *input-stack*)))
    (if (null? ipstk) #f
	(begin
	  (close-port (current-input-port))
	  (set-current-input-port (car ipstk))
	  (fluid-set! *input-stack* (cdr ipstk))))))

;; @deffn {Procedure} input-stack-portinfo
;; Return a list of pairs of input stack filename and line number.
;; @end deffn
(define (input-stack-portinfo)
  "- Procedure: input-stack-portinfo
     Return a list of pairs of input stack filename and line number."
  (define (port-info port)
    (cons (or (port-filename port) "(unknown)") (1+ (port-line port))))
  (fold-right (lambda (port info) (cons (port-info port) info)) '()
	      (fluid-ref *input-stack*)))

;; === tl ==============================

;; @section Tagged Lists
;; Tagged lists are
;; They are implemented as a cons cell with the car and the cdr a list.
;; The cdr is used to accumulate appended items and the car is used to
;; keep the tag, attributes and inserted items.
;; @example
;; tl => '(H . T), H => (c a b 'tag); T =>
;; @end example

;; @deffn {Procedure} make-tl tag [item item ...]
;; Create a tagged-list structure.
;; @end deffn
(define (make-tl tag . rest)
  "- Procedure: make-tl tag [item item ...]
     Create a tagged-list structure."
  (let loop ((tail tag) (l rest))
    (if (null? l) (cons '() tail)
	(loop (cons (car l) tail) (cdr l)))))

;; @deffn {Procedure} tl->list tl
;; Convert a tagged list structure to a list.  This collects added attributes
;; and puts them right after the (leading) tag, resulting in something like
;; @example
;; (<tag> (@ <attr>) <rest>)
;; @end example
;; @end deffn
(define (tl->list tl)
  "- Procedure: tl->list tl
     Convert a tagged list structure to a list.  This collects added
     attributes and puts them right after the (leading) tag, resulting
     in something like
          (<tag> ( <attr>) <rest>)"
  (let ((heda (car tl))
	(head (let loop ((head '()) (attr '()) (tl-head (car tl)))
		(if (null? tl-head)
		    (if (pair? attr)
			(cons (cons '@ attr) (reverse head))
			(reverse head))
		    (if (and (pair? (car tl-head)) (eq? '@ (caar tl-head)))
			(loop head (cons (cdar tl-head) attr) (cdr tl-head))
			(loop (cons (car tl-head) head) attr (cdr tl-head)))))))
    (let loop ((tail '()) (tl-tail (cdr tl)))
      (if (pair? tl-tail)
	  (loop (cons (car tl-tail) tail) (cdr tl-tail))
	  (cons tl-tail (append head tail))))))

;; @deffn {Procedure} tl-insert tl item
;; Insert item at front of tagged list (but after tag).
;; @end deffn
(define (tl-insert tl item)
  "- Procedure: tl-insert tl item
     Insert item at front of tagged list (but after tag)."
  (cons (cons item (car tl)) (cdr tl)))

;; @deffn {Procedure} tl-append tl item ...
;; Append items at end of tagged list.
;; @end deffn
(define (tl-append tl . rest)
  "- Procedure: tl-append tl item ...
     Append items at end of tagged list."
  (cons (car tl)
	(let loop ((tail (cdr tl)) (items rest))
	  (if (null? items) tail
	      (loop (cons (car items) tail) (cdr items))))))

;; @deffn {Procedure} tl-extend tl item-l
;; Extend with a list of items.
;; @end deffn
(define (tl-extend tl item-l)
  "- Procedure: tl-extend tl item-l
     Extend with a list of items."
  (apply tl-append tl item-l))

;; @deffn {Procedure} tl-extend! tl item-l
;; Extend with a list of items.  Uses @code{set-cdr!}.
;; @end deffn
(define (tl-extend! tl item-l)
  "- Procedure: tl-extend! tl item-l
     Extend with a list of items.  Uses 'set-cdr!'."
  (set-cdr! (last-pair tl) item-l)
  tl)

;; @deffn {Procedure} tl+attr tl key val)
;; Add an attribute to a tagged list.  Return a new tl.
;; @example
;; (tl+attr tl 'type "int")
;; @end example
;; @end deffn
(define (tl+attr tl key val)
  "- Procedure: tl+attr tl key val)
     Add an attribute to a tagged list.  Return a new tl.
          (tl+attr tl 'type \"int\")"
  (tl-insert tl (cons '@ (list key val))))

;; @deffn {Procedure} tl+attr tl key val [key val [@dots{} ...]]) => tl
;; Add multiple attributes to a tagged list.  Return a new tl.
;; @example
;; (tl+attr tl 'type "int")
;; @end example
;; @end deffn
(define (tl+attr* tl . rest)
  "- Procedure: tl+attr tl key val [key val [... ...]]) => tl
     Add multiple attributes to a tagged list.  Return a new tl.
          (tl+attr tl 'type \"int\")"
  (if (null? rest) tl
      (tl+attr* (tl+attr tl (car rest) (cadr rest)) (cddr rest))))

;; @deffn {Procedure} tl-merge tl tl1
;; Merge guts of phony-tl @code{tl1} into @code{tl}.
;; @end deffn
(define (tl-merge tl tl1)
  (error "tl-merge: not implemented (yet)"))

;;; === misc ========================

(define (mach-dir path file)
  (string-append path "/mach.d/" file))

;;; === pp ==========================
;; @section Pretty-Print and Other Utility Procedures

;; @deffn {Procedure} make-protect-expr op-prec op-assc => side op expr => #t|#f
;; Generate procedure @code{protect-expr} for pretty-printers, which takes
;; the form @code{(protect-expr? side op expr)} and where @code{side}
;; is @code{'lval} or @code{'rval}, @code{op} is the operator and @code{expr}
;; is the expression.  The argument @arg{op-prec} is a list of equivalent
;; operators in order of decreasing precedence and @arg{op-assc} is an
;; a-list of precedence with keys @code{'left}, @code{'right} and
;; @code{nonassoc}.
;; @example
;; (protect-expr? 'left '+ '(mul ...)) => TBD
;; @end example
;; @end deffn
(define (make-protect-expr op-prec op-assc)

  (define (assc-lt? op)
    (memq op (assq-ref op-assc 'left)))

  (define (assc-rt? op)
    (memq op (assq-ref op-assc 'right)))

  ;; @deffn {Procedure} prec a b => '>|'<|'=|#f
  ;; Returns the prececence relation of @code{a}, @code{b} as
  ;; @code{<}, @code{>}, @code{=} or @code{#f} (no relation).
  ;; @end deffn
  (define (prec a b)
    (let loop ((ag #f) (bg #f) (opg op-prec)) ;; a-group, b-group
      (cond
       ((null? opg) #f)			; indeterminate
       ((memq a (car opg))
	(if bg '<
	    (if (memq b (car opg)) '=
		(loop #t bg (cdr opg)))))
       ((memq b (car opg))
	(if ag '>
	    (if (memq a (car opg)) '=
		(loop ag #t (cdr opg)))))
       (else
	(loop ag bg (cdr opg))))))

  (lambda (side op expr)
    (let ((assc? (case side
		   ((lt lval left) assc-rt?)
		   ((rt rval right) assc-lt?)))
	  (vtag (car expr)))
      (case (prec op vtag)
	((>) #t)
	((<) #f)
	((=) (assc? op))
	(else #f)))))

;; @deffn {Procedure} expand-tabs str [col]
;; Expand tabs where the string @var{str} starts in column @var{col}
;; (default 0). 
;; @end deffn
(define* (expand-tabs str #:optional (col 0))

  (define (fill-tab col chl)
    (let loop ((chl (if (zero? col) (cons #\space chl) chl))
	       (col (if (zero? col) (1+ col) col)))
      (if (zero? (modulo col 8)) chl
	  (loop (cons #\space chl) (1+ col)))))

  (define (next-tab-col col) ;; TEST THIS !!!
    ;;(* 8 (quotient (+ 9 col) 8))) ???
    (* 8 (quotient col 8)))

  (let ((strlen (string-length str)))
    (let loop ((chl '()) (col col) (ix 0))
      (if (= ix strlen) (list->string (reverse chl))
	  (let ((ch (string-ref str ix)))
	    (case ch
	      ((#\newline)
	       (loop (cons ch chl) 0 (1+ ix)))
	      ((#\tab)
	       (loop (fill-tab col chl) (next-tab-col col) (1+ ix)))
	      (else
	       (loop (cons ch chl) (1+ col) (1+ ix)))))))))

;; @deffn {Procedure} make-pp-formatter [port] <[options> => fmtr
;; Options
;; @table @code
;; @item #:per-line-prefix
;; string to prefix each line
;; @item #:width
;; Max width of output.  Default is 79 columns.
;; @end itemize
;; @example
;; (fmtr 'push) ;; push indent level
;; (fmtr 'pop)  ;; pop indent level
;; (fmtr "fmt" arg1 arg2 ...)
;; @end example
;; @end deffn
(define* (make-pp-formatter #:optional (port (current-output-port))
			    #:key per-line-prefix (width 79) (basic-offset 2))
  (letrec*
      ((pfxlen (string-length (expand-tabs (or per-line-prefix ""))))
       (maxcol (- width (if per-line-prefix pfxlen 0)))
       (maxind 36)
       (column 0)
       (ind-lev 0)
       (ind-len 0)
       (blanks "                                            ")
       (ind-str (lambda () (substring blanks 0 ind-len)))
       (cnt-str (lambda () (substring blanks 0 (+ basic-offset 2 ind-len))))
       ;;(sf-nl (lambda () (newline) (set! column 0)))

       (push-il
	(lambda ()
	  (set! ind-lev (min maxind (1+ ind-lev)))
	  (set! ind-len (* basic-offset ind-lev))))

       (pop-il
	(lambda ()
	  (set! ind-lev (max 0 (1- ind-lev)))
	  (set! ind-len (* basic-offset ind-lev))))

       (inc-column!
	(lambda (inc)
	  (set! column (+ column inc))))

       (set-column!
	(lambda (val)
	  (set! column val)))
       
       (sf
	(lambda (fmt . args)
	  (let* ((str (apply simple-format #f fmt args))
		 (str (if (and (zero? column) per-line-prefix)
			  (begin
			    (when #f ;;(char=? #\tab (string-ref str 0))
			      (sferr "expand-tabs (pfxlen=~S)\n" pfxlen)
			      (sferr "~A\n" str)
			      (sferr "~A~A\n\n" per-line-prefix
				     (expand-tabs str pfxlen)))
			    (expand-tabs str pfxlen))
			  str))
		 (len (string-length str)))
	    (cond
	     ((zero? column)
	      (if per-line-prefix (display per-line-prefix port))
	      (display (ind-str) port)
	      (inc-column! ind-len))
	     ((> (+ column len) maxcol)
	      (newline port)
	      (if per-line-prefix (display per-line-prefix port))
	      (display (cnt-str) port)
	      (set-column! (+ ind-len 4))))
	    (display str port)
	    (inc-column! len)
	    (when (and (positive? len)
		       (eqv? #\newline (string-ref str (1- len))))
	      (set! column 0))))))

    (lambda (arg0 . rest)
      (cond
       ;;((string? arg0) (if (> (string-length arg0) 0) (apply sf arg0 rest)))
       ((string? arg0) (apply sf arg0 rest))
       ((eqv? 'push arg0) (push-il))
       ((eqv? 'pop arg0) (pop-il))
       ((eqv? 'nlin arg0) ;; newline if needed
        (cond ((positive? column) (newline) (set! column 0))))
       (else (throw 'nyacc-error "pp-formatter: bad args"))))))

;; @deffn {Procedure} make-pp-formatter/ugly => fmtr
;; Makes a @code{fmtr} like @code{make-pp-formatter} but no indentation
;; and just adds strings on ...
;; This is specific to C/C++ because it will newline if #\# seen first.
;; @end deffn
(define* (make-pp-formatter/ugly)
  (let*
      ((maxcol 78)
       (column 0)
       (sf (lambda (fmt . args)
	     (let* ((str (apply simple-format #f fmt args))
		    (len (string-length str)))
	       (if (and (positive? len)
			(char=? #\newline (string-ref str (1- len))))
		   (string-set! str (1- len) #\space))
	       (cond
		((zero? len) #t)	; we reference str[0] next
		((and (equal? len 1) (char=? #\newline (string-ref str 0))) #t)
		((char=? #\# (string-ref str 0)) ; CPP-stmt: force newline
		 (when (positive? column) (newline))
		 (display str)		; str always ends in \n
		 (set! column		; if ends \n then col= 0 else len
		       (if (char=? #\newline (string-ref str (1- len)))
			   0 len)))
		((zero? column)
		 (display str)
		 (set! column len))
		(else
		 (when (> (+ column len) maxcol)
		   (newline)
		   (set! column 0))
		 (display str)
		 (set! column (+ column len))))))))

    (lambda (arg0 . rest)
      (cond
       ((string? arg0) (apply sf arg0 rest))
       ((eqv? 'nlin arg0) ;; newline if needed
        (cond ((positive? column) (newline) (set! column 0))))
       ((eqv? 'push arg0) #f)
       ((eqv? 'pop arg0) #f)
       (else (throw 'nyacc-error "pp-formatter/ugly: bad args"))))))
  
;; @deffn {Procedure} move-if-changed src-file dst-file [sav-file]
;; Return @code{#t} if changed.
;; @end deffn
(define (move-if-changed src-file dst-file . rest)

  (define (doit)
    (let ((sav-file (if (pair? rest) (car rest) #f)))
      (if (and sav-file (access? sav-file W_OK))
	  (system (simple-format #f "mv ~A ~A" dst-file sav-file)))
      (system (simple-format #f "mv ~A ~A" src-file dst-file))
      #t))
    
  (cond
   ;; src-file does not exist
   ((not (access? src-file R_OK)) #f)

   ;; dst-file does not exist, update anyhow
   ((not (access? dst-file F_OK))
    (system (simple-format #f "mv ~A ~A" src-file dst-file)) #t)

   ;; both exist, but no changes
   ((zero? (system
	    (simple-format #f "cmp ~A ~A >/dev/null" src-file dst-file)))
    (system (simple-format #f "rm ~A" src-file)) #f)

   ;; both exist, update
   ((access? dst-file W_OK)
    (doit))
   
   (else
    (simple-format (current-error-port) "move-if-changed: no write access\n")
    #f)))

;; @deffn {Procedure} cintstr->scm str => #f|str
;; Convert a C string for a fixed type to a Scheme string.
;; If not identified as a C int, then return @code{#f}.
;; TODO: add support for character literals (and unicode?).
;; @end deffn
(define cs:dig (string->char-set "0123456789"))
(define cs:hex (string->char-set "0123456789ABCDEFabcdef"))
(define cs:oct (string->char-set "01234567"))
(define cs:long (string->char-set "lLuU"))
(define (cintstr->scm str)
  ;; dl=digits, ba=base, st=state, ix=index
  ;; 0: "0"->1, else->2
  ;; 1: "x"->(base 16)2, else->(base 8)2
  ;; 2: "0"-"9"->(cons ch dl), else->3:
  ;; 3: "L","l","U","u"->3, eof->(cleanup) else->#f
  (let ((ln (string-length str)))
    (let loop ((dl '()) (bx "") (cs cs:dig) (st 0) (ix 0))
      (if (= ix ln)
	  (if (null? dl) #f (string-append bx (list->string (reverse dl))))
	  (case st
	    ((0) (loop (cons (string-ref str ix) dl) bx cs
		       (if (char=? #\0 (string-ref str ix)) 1 2)
		       (1+ ix)))
	    ((1) (if (char=? #\x (string-ref str ix))
		     (loop '() "#x" cs:hex 2 (1+ ix))
		     (loop '() "#o" cs:oct 2 ix)))
	    ((2) (if (char-set-contains? cs (string-ref str ix))
		     (loop (cons (string-ref str ix) dl) bx cs st (1+ ix))
		     (if (char-set-contains? cs:long (string-ref str ix))
			 (loop dl bx cs 3 (1+ ix))
			 #f)))
	    ((3) #f))))))

;;; --- last line ---

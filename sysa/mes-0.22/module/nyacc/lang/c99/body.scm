;;; lang/c99/body.scm - parser body, inserted in parser.scm

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

;;; Notes:

;; Notes on the code design may be found in doc/nyacc/lang/c99-hg.info

;; @section The C99 Parser Body
;; This code provides the front end to the C99 parser, including the lexical
;; analyzer and optional CPP processing.  In @code{'file} mode the lex'er
;; passes CPP statements to the parser; in @code{'code} mode the lex'er
;; parses and evaluates the CPP statements.  In the case of included files
;; (e.g., via @code{#include <file.h>}) the include files are parsed if
;; not in @code{inc-help}.  The a-list @code{inc-help} maps
;; include file names to typenames (e.g., @code{stdio.h} to @code{FILE}) and
;; CPP defines (e.g., "INT_MAX=12344").

;; issue w/ brlev: not intended to beused with `extern "C" {'

;;; Code:

(use-modules (nyacc lang sx-util))
(use-modules (nyacc lang util))
(use-modules ((srfi srfi-1) #:select (fold-right append-reverse)))
(use-modules ((srfi srfi-9) #:select (define-record-type)))
(use-modules (ice-9 pretty-print))	; for debugging
(define (sf fmt . args) (apply simple-format #t fmt args))
(define pp pretty-print)

;; C parser info (?)
(define-record-type cpi
  (make-cpi-1)
  cpi?
  (debug cpi-debug set-cpi-debug!)	; debug #t #f
  (shinc cpi-shinc set-cpi-shinc!)	; show includes
  (defines cpi-defs set-cpi-defs!)	; #defines
  (incdirs cpi-incs set-cpi-incs!)	; #includes
  (inc-tynd cpi-itynd set-cpi-itynd!)	; a-l of incfile => typenames
  (inc-defd cpi-idefd set-cpi-idefd!)	; a-l of incfile => defines
  (ptl cpi-ptl set-cpi-ptl!)		; parent typename list
  (ctl cpi-ctl set-cpi-ctl!)		; current typename list
  (blev cpi-blev set-cpi-blev!)		; curr brace/block level
  )

;;.@deffn Procedure split-cppdef defstr => (<name> . <repl>)| \
;;     (<name>  <args> . <repl>)|#f
;; Convert define string to a dict item.  Examples:
;; @example
;; "ABC=123" => '("ABC" . "123")
;; "MAX(X,Y)=((X)>(Y)?(X):(Y))" => ("MAX" ("X" "Y") . "((X)>(Y)?(X):(Y))")
;; @end example
;; @end deffn
(define (split-cppdef defstr)
  (let ((x2st (string-index defstr #\()) ; start of args
	(x2nd (string-index defstr #\))) ; end of args
	(x3 (string-index defstr #\=)))  ; start of replacement
    (cond
     ((not x3) #f)
     ((and x2st x3)
      ;;(if (not (eq? (1+ x2nd) x3)) (c99-err "bad CPP def: ~S" defstr))
      (cons* (substring defstr 0 x2st)
	     (string-split
	      (string-delete #\space (substring defstr (1+ x2st) x2nd))
	      #\,)
	     (substring defstr (1+ x3))))
     (else
      (cons (substring defstr 0 x3) (substring defstr (1+ x3)))))))

;; @deffn Procedure make-cpi debug defines incdirs inchelp
;; I think there is a potential bug here in that the alist of cpp-defs/helpers
;; should be last-in-first-seen ordered.  Probably helpers low prio.
;; The (CPP) defines can appear as pairs: then they have already been split.
;; (This is used by @code{parse-c99x}.)
;; @end deffn
(define (make-cpi debug shinc defines incdirs inchelp)
  ;; convert inchelp into inc-file->typenames and inc-file->defines
  ;; Any entry for an include file which contains `=' is considered
  ;; a define; otherwise, the entry is a typename.

  (define (split-helper helper)
    (let ((file (car helper)))
      (let loop ((tyns '()) (defs '()) (ents (cdr helper)))
	(cond
	 ((null? ents) (values (cons file tyns) (cons file defs)))
	 ((split-cppdef (car ents)) =>
	  (lambda (def) (loop tyns (cons def defs) (cdr ents))))
	 (else (loop (cons (car ents) tyns) defs (cdr ents)))))))

  (define (split-if-needed def)
    (if (pair? def) def (split-cppdef def)))

  (let* ((cpi (make-cpi-1)))
    (set-cpi-debug! cpi debug)		; print states debug 
    (set-cpi-shinc! cpi shinc)		; print includes
    (set-cpi-defs! cpi (map split-if-needed defines)) ; def's as pairs
    (set-cpi-incs! cpi incdirs)		; list of include dir's
    (set-cpi-ptl! cpi '())		; list of lists of typenames
    (set-cpi-ctl! cpi '())		; list of current typenames
    (set-cpi-blev! cpi 0)		; brace/block level
    ;; Break up the helpers into typenames and defines.
    (let loop ((itynd '()) (idefd '()) (helpers inchelp))
      (cond ((null? helpers)
	     (set-cpi-itynd! cpi itynd)
	     (set-cpi-idefd! cpi idefd))
	    (else
	     (call-with-values
		 (lambda () (split-helper (car helpers)))
	       (lambda (ityns idefs)
		 (loop (cons ityns itynd) (cons idefs idefd) (cdr helpers)))))))
    ;; Assign builtins.
    (and=> (assoc-ref (cpi-itynd cpi) "__builtin")
	   (lambda (tl) (set-cpi-ctl! cpi (append tl (cpi-ctl cpi)))))
    (and=> (assoc-ref (cpi-idefd cpi) "__builtin")
	   (lambda (tl) (set-cpi-defs! cpi (append tl (cpi-defs cpi)))))
    cpi))

(define *info* (make-fluid))
	  
(define cpi-inc-blev!
  (case-lambda
    ((info) (set-cpi-blev! info (1+ (cpi-blev info))))
    (() (cpi-inc-blev! (fluid-ref *info*)))))
(define cpi-dec-blev!
  (case-lambda
    ((info) (set-cpi-blev! info (1- (cpi-blev info))))
    (() (cpi-dec-blev! (fluid-ref *info*)))))
(define cpi-top-blev?
  (case-lambda
    ((info) (zero? (cpi-blev info)))
    (() (cpi-top-blev? (fluid-ref *info*)))))

(define cpi-push
  (case-lambda
    ((info) 
     (set-cpi-ptl! info (cons (cpi-ctl info) (cpi-ptl info)))
     (set-cpi-ctl! info '())
     #t)
    (() (cpi-push (fluid-ref *info*)))))

(define cpi-pop
  (case-lambda
    ((info)
     (set-cpi-ctl! info (car (cpi-ptl info)))
     (set-cpi-ptl! info (cdr (cpi-ptl info)))
     #t)
    (() (cpi-pop (fluid-ref *info*)))))

(define (cpi-push-x)	;; on #if
  ;;(sf "\ncpi-push-x:\n") (pp (fluid-ref *info*))
  (let ((cpi (fluid-ref *info*)))
    (set-cpi-ptl! cpi (cons (cpi-ctl cpi) (cpi-ptl cpi)))
    (set-cpi-ctl! cpi '())))

(define (cpi-shift-x)	;; on #elif #else
  ;;(sf "\ncpi-shift-x:\n") (pp (fluid-ref *info*))
  (set-cpi-ctl! (fluid-ref *info*) '()))

(define (cpi-pop-x)	;; on #endif
  ;;(sf "\ncpi-pop-x:\n") (pp (fluid-ref *info*))
  (let ((cpi (fluid-ref *info*)))
    (set-cpi-ctl! cpi (append (cpi-ctl cpi) (car (cpi-ptl cpi))))
    (set-cpi-ptl! cpi (cdr (cpi-ptl cpi)))))

;; @deffn {Procedure} typename? name
;; Called by lexer to determine if symbol is a typename.
;; Check current sibling for each generation.
;; @end deffn
(define (typename? name)
  (let ((cpi (fluid-ref *info*)))
    (if (member name (cpi-ctl cpi)) #t
        (let loop ((ptl (cpi-ptl cpi)))
	  (if (null? ptl) #f
	      (if (member name (car ptl)) #t
		  (loop (cdr ptl))))))))

;; @deffn {Procedure} add-typename name
;; Helper for @code{save-typenames}.
;; @end deffn
(define (add-typename name)
  (let ((cpi (fluid-ref *info*)))
    (set-cpi-ctl! cpi (cons name (cpi-ctl cpi)))))

;; @deffn {Procedure} find-new-typenames decl
;; Helper for @code{save-typenames}.
;; Given declaration return a list of new typenames (via @code{typedef}).
;; @end deffn
(define (find-new-typenames decl)
  ;; like declr-id in util2.scm
  (define (declr->id-name declr)
    (case (car declr)
      ((ident) (sx-ref declr 1))
      ((init-declr) (declr->id-name (sx-ref declr 1)))
      ((comp-declr) (declr->id-name (sx-ref declr 1)))
      ((array-of) (declr->id-name (sx-ref declr 1)))
      ((ptr-declr) (declr->id-name (sx-ref declr 2)))
      ((ftn-declr) (declr->id-name (sx-ref declr 1)))
      ((scope) (declr->id-name (sx-ref declr 1)))
      (else (error "coding bug: " declr))))
       
  ;;(sf "\ndecl:\n") (pp decl)

  (let* ((spec (sx-ref decl 1))
	 (stor (sx-find 'stor-spec spec))
	 (id-l (sx-ref decl 2)))
    (if (and stor (eqv? 'typedef (caadr stor)))
	(let loop ((res '()) (idl (cdr id-l)))
	  (if (null? idl) res
	      (loop (cons (declr->id-name (sx-ref (car idl) 1)) res)
		    (cdr idl))))
	'())))

;; @deffn {Procedure} save-typenames decl
;; Save the typenames for the lexical analyzer and return the decl.
;; @end deffn
(define (save-typenames decl)
  ;; This finds typenames using @code{find-new-typenames} and adds via
  ;; @code{add-typename}.  Then return the decl.
  (for-each add-typename (find-new-typenames decl))
  decl)

;; (string "abc" "def") -> (string "abcdef")
;; In the case that declaration-specifiers only returns a list of
;; attribute-specifiers then this has to be an empty-statemnet with
;; attributes.  See:
;;   https://gcc.gnu.org/onlinedocs/gcc-8.2.0/gcc/Statement-Attributes.html
(define (XXX-only-attr-specs? specs)
  (let loop ((specs specs))
    (cond
     ((null? specs) #t)
     ((not (eqv? 'attributes (sx-tag (car specs)))) #f)
     (else (loop (cdr specs))))))

;; ------------------------------------------------------------------------

(define (c99-err . args)
  (apply throw 'c99-error args))

;; @deffn {Procedure} read-cpp-line ch => #f | (cpp-xxxx)??
;; Given if ch is #\# read a cpp-statement.
;; The standard implies that comments are tossed here but we keep them
;; so that they can end up in the pretty-print output.
;; @end deffn
(define (read-cpp-line ch)
  (if (not (eq? ch #\#)) #f
      (let loop ((cl '()) (ch (read-char)))
	(cond
	 ;;((eof-object? ch) (throw 'cpp-error "CPP lines must end in newline"))
	 ((eof-object? ch) (reverse-list->string cl))
	 ((eq? ch #\newline) (unread-char ch) (reverse-list->string cl))
	 ((eq? ch #\\)
	  (let ((c2 (read-char)))
	    (if (eq? c2 #\newline)
		(loop cl (read-char))
		(loop (cons* c2 ch cl) (read-char)))))
	 ((eq? ch #\/) ;; swallow comments, even w/ newlines
	  (let ((c2 (read-char)))
	    (cond
	     ((eqv? c2 #\*)
	      (let loop2 ((cl2 (cons* #\* #\/ cl)) (ch (read-char)))
		(cond
		 ((eq? ch #\*)
		  (let ((c2 (read-char)))
		    (if (eqv? c2 #\/)
			(loop (cons* #\/ #\* cl2) (read-char)) ;; keep comment
			(loop2 (cons #\* cl2) c2))))
		 (else
		  (loop2 (cons ch cl2) (read-char))))))
	     (else
	      (loop (cons #\/ cl) c2)))))
	 (else (loop (cons ch cl) (read-char)))))))

(define (def-xdef? name mode)
  (not (eqv? mode 'file)))

  
;; @deffn {Procedure} make-c99-lexer-generator match-table raw-parser => proc
;; This generates a procedure which has the signature
;; @example
;; proc [#:mode mode] [#:xdef? proc] => procedure
;; @end example
;; to be passed to the c99 parsers.
;; The proc will generate a context-sensitive lexer for the C99 language.
;; The arg @var{match-table} is an element of a specification returned
;; by @code{make-lalr-spec} or machine generated by @code{make-lalr-machine}.
;; The argument @var{raw-parse} must be ...
;; The generated
;; lexical analyzer reads and passes comments and optionally CPP statements
;; to the parser.  The keyword argument @var{mode} will determine if CPP
;; statements are passed (@code{'file} mode) or parsed and executed
;; (@code{'file} mode) as described above.  Comments will be passed as
;; ``line'' comments or ``lone'' comments: lone comments appear on a line
;; without code.  The @code{xdef?} keyword argument allows user to pass
;; a predicate which determines whether CPP symbols in code are expanded.
;; The default predicate is
;; @example
;; (define (def-xdef? mode name) (eqv? mode 'code))
;; @end example
;; @end deffn
(define (make-c99-lexer-generator match-table raw-parser)
  ;; This gets ugly in order to handle cpp.  The CPP will tokenize, expand,
  ;; then convert back to a string.
  ;;
  ;; todo: check if @code{1.3f} gets parsed as a number.
  ;; todo: I think there is a bug wrt the comment reader because // ... \n
  ;; will end up in same mode...  so after
  ;; int x; // comment
  ;; the lexer will think we are not at BOL.
  ;;
  ;; The state variable `suppress' is used to suppress re-expansion of input
  ;; text generated by the CPP macro expander.  The CPP replacement text
  ;; inserted via a string-port on the port stack.  When that port is fully
  ;; read (i.e., the reader sees eof-object) then @var{suppress} is changed
  ;; to @code{#t}.

  (define (getdefs stmts)		; extract defines
    (fold-right
     (lambda (stmt seed)
       ;;(sx-match stmt
       ;;  ((cpp-stmt (define . ,rest)) (cons (sx-ref stmt 1) seed))
       ;;  (else seed)))
       (if (and (eqv? 'cpp-stmt (sx-tag stmt))
		(eqv? 'define (sx-tag (sx-ref stmt 1))))
	   (cons (sx-ref stmt 1) seed)
	   seed))
     '() stmts))
  
  (let* ((ident-like? (make-ident-like-p read-c-ident))
	 ;;
	 (strtab (filter-mt string? match-table)) ; strings in grammar
	 (kwstab (filter-mt ident-like? strtab))  ; keyword strings =>
	 (keytab (map-mt string->symbol kwstab))  ; keywords in grammar
	 (chrseq (remove-mt ident-like? strtab))  ; character sequences
	 (symtab (filter-mt symbol? match-table)) ; symbols in grammar
	 (chrtab (filter-mt char? match-table))	  ; characters in grammar
	 ;;
	 (read-chseq (make-chseq-reader chrseq))
	 (assc-$ (lambda (pair)
		   (cons (assq-ref symtab (car pair)) (cdr pair))))
	 ;;
	 (t-ident (assq-ref symtab '$ident))
	 (t-typename (assq-ref symtab 'typename)))

    ;; mode: 'code|'file|'decl
    ;; xdef?: (proc name mode) => #t|#f  : do we expand #define?
    ;;(lambda* (#:key (mode 'code) xdef? show-incs)
    (define* (lexer #:key (mode 'code) xdef? show-incs)

      (define (run-parse)
	(let ((info (fluid-ref *info*)))
	  (raw-parser (lexer #:mode 'decl #:show-incs (cpi-shinc info))
		      #:debug (cpi-debug info))))
      
      (let ((bol #t)		 ; begin-of-line condition
	    (suppress #f)	 ; parsing cpp expanded text (kludge?)
	    (ppxs (list 'keep))	 ; CPP execution state stack
	    (info (fluid-ref *info*))	; info shared w/ parser
	    ;;(brlev 0)			; brace level
	    (x-def? (cond ((procedure? xdef?) xdef?)
			  ((eq? xdef? #t) (lambda (n m) #t))
			  (else def-xdef?))))
	;; Return the first (tval . lval) pair not excluded by the CPP.
	(lambda ()

	  (define (add-define tree)
	    (let* ((tail (cdr tree))
		   (name (car (assq-ref tail 'name)))
		   (args (assq-ref tail 'args))
		   (repl (car (assq-ref tail 'repl)))
		   (cell (cons name (if args (cons args repl) repl))))
	      (set-cpi-defs! info (cons cell (cpi-defs info)))))
	  
	  (define (rem-define name)
	    (set-cpi-defs! info (acons name #f (cpi-defs info))))
	  
	  (define (apply-helper file)
	    ;; file will include <> or "", need to strip
	    (let* ((tyns (assoc-ref (cpi-itynd info) file))
		   (defs (assoc-ref (cpi-idefd info) file)))
	      (when tyns
		(for-each add-typename tyns)
		(set-cpi-defs! info (append defs (cpi-defs info))))
	      tyns))

	  (define (inc-stmt->file-spec stmt) ;; retain <> or ""
	    (let* ((arg (cadr stmt)))
	      (if (ident-like? arg) ;; #include MYFILE
		  (expand-cpp-macro-ref arg (cpi-defs info))
		  arg)))

	  (define (file-spec->file spec)
	    (substring/shared spec 1 (1- (string-length spec))))

	  (define (inc-file-spec->path spec next)
	    (find-incl-in-dirl spec (cpi-incs info) next))

	  (define (code-if stmt)
	    (case (car ppxs)
	      ((skip-look skip-done skip) ;; don't eval if excluded
	       (set! ppxs (cons 'skip ppxs)))
	      (else
	       (let* ((defs (cpi-defs info))
		      (val (eval-cpp-cond-text (cadr stmt) defs
					       #:inc-dirs (cpi-incs info))))
		 (if (not val) (c99-err "unresolved: ~S" (cadr stmt)))
		 (if (eq? 'keep (car ppxs))
		     (if (zero? val)
			 (set! ppxs (cons 'skip-look ppxs))
			 (set! ppxs (cons 'keep ppxs)))
		     (set! ppxs (cons 'skip-done ppxs))))))
	    stmt)

	  (define (code-elif stmt)
	    (case (car ppxs)
	      ((skip) #t) ;; don't eval if excluded
	      (else
	       (let* ((defs (cpi-defs info))
		      (val (eval-cpp-cond-text (cadr stmt) defs
					       #:inc-dirs (cpi-incs info))))
		 (if (not val) (c99-err "unresolved: ~S" (cadr stmt)))
		 (case (car ppxs)
		   ((skip-look) (if (not (zero? val)) (set-car! ppxs 'keep)))
		   ((keep) (set-car! ppxs 'skip-done))))))
	    stmt)

	  (define (code-else stmt)
	    (case (car ppxs)
	      ((skip-look) (set-car! ppxs 'keep))
	      ((keep) (set-car! ppxs 'skip-done)))
	    stmt)

	  (define (code-endif stmt)
	    (set! ppxs (cdr ppxs))
	    stmt)
	  
	  (define* (eval-cpp-incl/here stmt #:optional next) ;; => stmt
	    (let* ((spec (inc-stmt->file-spec stmt))
		   (file (file-spec->file spec))
		   (path (inc-file-spec->path spec next)))
	      (if show-incs (sferr "include ~A => ~S\n" spec path))
	      (cond
	       ((apply-helper file) stmt)
	       ((not path) (c99-err "not found: ~S" file))
	       (else (set! bol #t)
		     (push-input (open-input-file path))
		     (if path (sx-attr-add stmt 'path path) stmt)))))

	  (define* (eval-cpp-incl/tree stmt #:optional next) ;; => stmt
	    ;; include file as a new tree
	    (let* ((spec (inc-stmt->file-spec stmt))
		   (file (file-spec->file spec))
		   (path (inc-file-spec->path spec next)))
	      (if show-incs (sferr "include ~A => ~S\n" spec path))
	      (cond
	       ((apply-helper file) stmt)
	       ((not path) (c99-err "not found: ~S" file))
	       ((with-input-from-file path run-parse) =>
		(lambda (tree) ;; add tree
		  (for-each add-define (getdefs tree))
		  (append (if path (sx-attr-add stmt 'path path) stmt)
			  (list tree)))))))

	  (define (eval-cpp-stmt/code stmt) ;; => stmt
	    (case (car stmt)
	      ((if) (code-if stmt))
	      ((elif) (code-elif stmt))
	      ((else) (code-else stmt))
	      ((endif) (code-endif stmt))
	      (else
	       (if (eqv? 'keep (car ppxs))
		   (case (car stmt)
		     ((include) (eval-cpp-incl/here stmt))
		     ((include-next) (eval-cpp-incl/here stmt 'next))
		     ((define) (add-define stmt) stmt)
		     ((undef) (rem-define (cadr stmt)) stmt)
		     ((error) (c99-err "error: #error ~A" (cadr stmt)))
		     ((warning) (report-error "warning: ~A" (cdr stmt)))
		     ((pragma) stmt)
		     ((line) stmt)
		     (else
		      (sferr "stmt: ~S\n" stmt)
		      (error "nyacc eval-cpp-stmt/code: bad cpp flow stmt")))
		   stmt))))
	
	  (define (eval-cpp-stmt/decl stmt) ;; => stmt
	    (case (car stmt)
	      ((if) (code-if stmt))
	      ((elif) (code-elif stmt))
	      ((else) (code-else stmt))
	      ((endif) (code-endif stmt))
	      (else
	       (if (eqv? 'keep (car ppxs))
		   (case (car stmt)
		     ((include)		; use tree unless inside braces
		      (if (cpi-top-blev? info)
			  (eval-cpp-incl/tree stmt)
			  (eval-cpp-incl/here stmt)))
		     ((include-next)	; gcc extension
		      (if (cpi-top-blev? info)
			  (eval-cpp-incl/tree stmt 'next)
			  (eval-cpp-incl/here stmt 'next)))
		     ((define) (add-define stmt) stmt)
		     ((undef) (rem-define (cadr stmt)) stmt)
		     ((error) (c99-err "error: #error ~A" (cadr stmt)))
		     ((warning) (report-error "warning: ~A" (cdr stmt)) stmt)
		     ((pragma) stmt) ;; ignore for now
		     ((line) stmt)
		     (else
		      (sferr "stmt: ~S\n" stmt)
		      (error "eval-cpp-stmt/decl: bad cpp flow stmt")))
		   stmt))))
	  
	  (define (eval-cpp-stmt/file stmt) ;; => stmt
	    (case (car stmt)
	      ((if) (cpi-push-x) stmt)
	      ((elif else) (cpi-shift-x) stmt)
	      ((endif) (cpi-pop-x) stmt)
	      ((include) (eval-cpp-incl/tree stmt))
	      ((define) (add-define stmt) stmt)
	      ((undef) (rem-define (cadr stmt)) stmt)
	      ((error) stmt)
	      ((warning) stmt)
	      ((pragma) stmt)
	      ((line) stmt)
	      (else
	       (sferr "stmt: ~S\n" stmt)
	       (error "eval-cpp-stmt/file: bad cpp flow stmt"))))

	  ;; Maybe evaluate the CPP statement.
	  (define (eval-cpp-stmt stmt)
	    (with-throw-handler
		'cpp-error
	      (lambda ()
		(case mode
		  ((code) (eval-cpp-stmt/code stmt))
		  ((decl) (eval-cpp-stmt/decl stmt))
		  ((file) (eval-cpp-stmt/file stmt))
		  (else (error "nyacc eval-cpp-stmt: coding error"))))
	      (lambda (key fmt . rest)
		(report-error fmt rest)
		(throw 'c99-error "CPP error"))))

	  ;; Predicate to determine if we pass the cpp-stmt to the parser.
	  ;; @itemize
	  ;; If code mode, never
	  ;; If file mode, all except includes between { }
	  ;; If decl mode, only defines and includes outside {}
	  ;; @end itemize
	  (define (pass-cpp-stmt stmt)
	    (if (eq? 'pragma (car stmt))
		(if (eq? mode 'file)
		    `(cpp-stmt ,stmt)
		    `($pragma . ,(cadr stmt)))
		(case mode
		  ((code) #f)
		  ((decl) (and (cpi-top-blev? info)
			       (memq (car stmt) '(include define include-next))
			       `(cpp-stmt . ,stmt)))
		  ((file) (and
			   (or (cpi-top-blev? info)
			       (not (memq (car stmt) '(include include-next))))
			   `(cpp-stmt . ,stmt)))
		  (else (error "nyacc pass-cpp-stmt: coding error")))))

	  ;; Composition of @code{read-cpp-line} and @code{eval-cpp-line}.
	  (define (read-cpp-stmt ch)
	    (and=> (read-cpp-line ch) cpp-line->stmt))

	  (define (read-token)
	    (let loop ((ch (read-char)))
	      (cond
	       ((eof-object? ch)
		(set! suppress #f)
		(if (pop-input)
		    (loop (read-char))
		    (assc-$ '($end . "#<eof>"))))
	       ((eq? ch #\newline) (set! bol #t) (loop (read-char)))
	       ((char-set-contains? c:ws ch) (loop (read-char)))
	       (bol
		(set! bol #f)
		(cond ;; things that require bol
 		 ((read-c-comm ch #t #:skip-prefix #t) => assc-$)
		 ((read-cpp-stmt ch) =>
		  (lambda (stmt)
		    (cond ((pass-cpp-stmt (eval-cpp-stmt stmt)) => assc-$)
			  (else (loop (read-char))))))
		 (else (loop ch))))
	       ((read-c-chlit ch) => assc-$) ; before ident for [ULl]'c'
	       ((read-c-ident ch) =>
		(lambda (name)
		  (let ((symb (string->symbol name))
			(defs (cpi-defs info)))
		    (cond
		     ((and (not suppress)
			   (x-def? name mode)
			   (expand-cpp-macro-ref name defs))
		      => (lambda (repl)
			   (set! suppress #t) ; don't rescan
			   (push-input (open-input-string repl))
			   (loop (read-char))))
		     ((assq-ref keytab symb)
		      ;;^minor bug: won't work on #define keyword xxx
		      ;; try (and (not (assoc-ref name defs))
		      ;;          (assq-ref keytab symb))
		      => (lambda (t) (cons t name)))
		     ((typename? name)
		      (cons t-typename name))
		     (else
		      (cons t-ident name))))))
	       ((read-c-num ch) => assc-$)
	       ((read-c-string ch) => assc-$)
	       ((read-c-comm ch #f #:skip-prefix #t) => assc-$)
	       ;; Keep track of brace level and scope for typedefs.
	       ((and (char=? ch #\{)
		     (eqv? 'keep (car ppxs)) (cpi-inc-blev! info)
		     #f) #f)
	       ((and (char=? ch #\})
		     (eqv? 'keep (car ppxs)) (cpi-dec-blev! info)
		     #f) #f)
	       ((read-chseq ch) => identity)
	       ((assq-ref chrtab ch) => (lambda (t) (cons t (string ch))))
	       ((eqv? ch #\\) ;; C allows \ at end of line to continue
		(let ((ch (read-char)))
		  (cond ((eqv? #\newline ch) (loop (read-char))) ;; extend line
			(else (unread-char ch) (cons #\\ "\\"))))) ;; parse err
	       (else (cons ch (string ch))))))

	  ;; Loop between reading tokens and skipping tokens via CPP logic.
	  (let loop ((pair (read-token)))
	    ;;(report-error "lx loop=>~S" (list pair))
	    (case (car ppxs)
	      ((keep)
	       pair)
	      ((skip-done skip-look skip)
	       (loop (read-token)))
	      (else (error "make-c99-lexer-generator: coding error")))))))

    lexer))

;; --- last line ---

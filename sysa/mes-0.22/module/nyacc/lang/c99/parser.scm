;;; nyacc/lang/c99/parser.scm - C parser execution

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

(define-module (nyacc lang c99 parser)
  #:export (parse-c99 parse-c99x gen-c99-lexer gen-c99x-lexer gen-c-lexer)
  #:use-module (nyacc lex)
  #:use-module (nyacc parse)
  #:use-module (nyacc lang util)
  #:use-module (nyacc lang c99 cpp)
  #:use-module (nyacc lang c99 util)
  #:re-export (c99-def-help c99-std-help))
(cond-expand
  (guile-3)
  (guile-2)
  (guile
   (use-modules (srfi srfi-16))
   (use-modules (ice-9 optargs))
   (use-modules (ice-9 syncase))
   (use-modules (nyacc compat18)))
  (else))

(include-from-path "nyacc/lang/c99/body.scm")

;; Routines to process specifier-lists and declarators, indended
;; to provide option to convert attribute-specifiers elements into
;; SXML attributes.  See move-attributes in util.scm.
;;(define process-specs identity)
;;(define process-declr identity)
(define process-specs move-attributes)
(define process-declr move-attributes)

;; === file parser ====================

(include-from-path "nyacc/lang/c99/mach.d/c99-act.scm")
(include-from-path "nyacc/lang/c99/mach.d/c99-tab.scm")

(define c99-raw-parser
  (make-lalr-parser
   (acons 'act-v c99-act-v c99-tables)
   #:skip-if-unexp '($lone-comm $code-comm $pragma)))
	      
(define gen-c99-lexer
  (make-c99-lexer-generator c99-mtab c99-raw-parser))

;; @deffn {Procedure} parse-c99 [options]
;; where options are
;; @table code
;; @item #:cpp-defs @i{defs-list}
;; @i{defs-list} is a list of strings where each string is of the form
;; @i{NAME} or @i{NAME=VALUE}.
;; @item #:inc-dirs @i{dir-list}
;; @{dir-list} is a list of strings of paths to look for directories.
;; @item #:inc-help @i{helpers}
;; @i{helpers} is an a-list where keys are include files (e.g.,
;; @code{"stdint.h"}) and the value is a list of type aliases or CPP define
;; (e.g., @code{"foo_t" "FOO_MAX=3"}).
;; @item #:mode @i{mode}
;; @i{mode} is one literal @code{'code}, @code{'file}, or @code{'decl}.
;; The default mode is @code{'code}.
;; @item #:debug @i{bool}
;; a boolean which if true prints states from the parser
;; @end table
;; @example
;; (with-input-from-file "abc.c"
;;   (parse-c #:cpp-defs '("ABC=123"))
;;            #:inc-dirs '(("." "./incs" "/usr/include"))
;;            #:inc-help (append '("myinc.h" "foo_t" "bar_t") c99-std-help)
;;            #:mode 'file))
;; @end example
;; Note: for @code{file} mode user still needs to make sure CPP conditional
;; expressions can be fully evaluated, which may mean adding compiler generated
;; defines (e.g., using @code{gen-cpp-defs}).
;; @end deffn
(define* (parse-c99 #:key
		    (cpp-defs '())	    ; CPP defines
		    (inc-dirs '())	    ; include dirs
		    (inc-help c99-def-help) ; include helpers
		    (mode 'code)	; mode: 'file, 'code or 'decl
		    (xdef? #f)		; pred to determine expand
		    (show-incs #f)	; show include files
		    (debug #f))		; debug
  (let ((info (make-cpi debug show-incs cpp-defs (cons "." inc-dirs) inc-help)))
    (with-fluids ((*info* info)
		  (*input-stack* '()))
      (catch 'c99-error
	(lambda ()
	  (catch 'nyacc-error
	    (lambda () (c99-raw-parser
			(gen-c99-lexer #:mode mode
				       #:xdef? xdef?
				       #:show-incs show-incs)
			#:debug debug))
	    (lambda (key fmt . args) (apply throw 'c99-error fmt args))))
	(lambda (key fmt . args)
	  (report-error fmt args)
	  #f)))))

;; === expr parser ====================

(include-from-path "nyacc/lang/c99/mach.d/c99x-act.scm")
(include-from-path "nyacc/lang/c99/mach.d/c99x-tab.scm")

(define c99x-raw-parser
  (make-lalr-parser
   (acons 'act-v c99x-act-v c99x-tables)
   #:skip-if-unexp '($lone-comm $code-comm $pragma)))

(define gen-c99x-lexer
  (make-c99-lexer-generator c99x-mtab c99x-raw-parser))
  
;; @deffn {Procedure} parse-c99x string [typenames] [options]
;; where @var{string} is a string C expression, @var{typenames}
;; is a list of strings to be treated as typenames
;; and @var{options} may be any of
;; @table
;; @item cpp-defs
;; a list of strings to be treated as preprocessor definitions
;; @item xdef?
;; this argument can be a boolean a predicate taking a string argument
;; @item debug
;; a boolean which if true prints states from the parser
;; @end table
;; This needs to be explained in some detail.
;; [tyns '("foo_t")]
;; @end deffn
(define* (parse-c99x expr-string
		     #:optional
		     (tyns '())		; defined typenames
		     #:key
		     (cpp-defs '())	; CPP defines
		     (xdef? #f)		; pred to determine expand
		     (debug #f))	; debug?
  (let ((info (make-cpi debug #f cpp-defs '(".") '())))
    (set-cpi-ptl! info (cons tyns (cpi-ptl info)))
    (with-fluids ((*info* info)
		  (*input-stack* '()))
      (with-input-from-string expr-string
	(lambda ()
	  (catch 'c99-error
	    (lambda ()
	      (catch 'nyacc-error
		(lambda ()
		  (c99x-raw-parser (gen-c99x-lexer #:mode 'code #:xdef? xdef?)
				   #:debug debug))
		(lambda (key fmt . args)
		  (apply throw 'c99-error fmt args))))
	    (lambda (key fmt . rest)
	      (report-error fmt rest)
	      #f)))))))

;; --- last line ---

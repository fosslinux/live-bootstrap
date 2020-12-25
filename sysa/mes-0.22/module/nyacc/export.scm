;;; nyacc/export.scm

;; Copyright (C) 2015,2017-2018 Matthew R. Wette
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
;; You should have received a copy of the licence with this software.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(define-module (nyacc export)
  #:export (lalr->bison
	    lalr->guile
	    c-char token->bison elt->bison)
  #:use-module ((nyacc lalr) #:select (find-terminal pp-rule lalr-start))
  #:use-module (nyacc lex)
  #:use-module (nyacc util)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module ((srfi srfi-43) #:select (vector-for-each))
  #:use-module (ice-9 regex))

;; The code below, for exporting to guile and bison, should be moved to
;; an "export" module.

;; terminal:
;; ident-like-string -> caps
;; non-ident-like-string -> ChSeq_#_# ...
;; symbol -> if $, use _, otherwise ???

;; breakdown:
;; 1 terminal, or non-terminal:
;; 2 if non-terminal,
;;   replace - with _, replace $ with _
;; 3 if terminal, (output of @code{find-terminal})
;;   if symbol, use 2
;;   replace char with (c-char .)
;;   if length-1 string replace with (c-char .)
;;   if like-c-ident string, replace with CAPS
;;   otherwise use ChSeq

(define re/g regexp-substitute/global)

(define (chseq->name cs)
  (let* ((iseq (string-fold (lambda (c s) (cons* (char->integer c) s)) '() cs))
	 (tail (string-join (map number->string iseq) "_"))
	 (name (string-append "ChSeq_" tail)))
    name))

;; Convert char to string that works inside single quotes for C.
(define (c-char ch)
  (case ch
    ((#\') "'\\''")
    ((#\\) "'\\\\'")
    ((#\newline) "'\\n'")
    ((#\tab) "'\\t'")
    ((#\return) "\\r")
    (else (string #\' ch #\'))))

(define (token->bison tok)
  (cond
   ((eqv? tok '$error) "error")
   ((symbol? tok) (symbol->bison tok))
   ((char? tok) (c-char tok))
   ((string? tok)
    (cond
     ((like-c-ident? tok) (string-upcase tok))
     ((= 1 (string-length tok)) (c-char (string-ref tok 0)))
     (else (chseq->name tok))))
   (else (error "what?"))))

(define (symbol->bison symb)
  (let* ((str0 (symbol->string symb))
	 (str1 (re/g #f "-" str0 'pre "_" 'post))
	 (str2 (re/g #f "\\$" str1 'pre "_" 'post)))
    str2))

(define (elt->bison symb terms)
  (let ((term (find-terminal symb terms)))
    (if term
	(token->bison term)
	(symbol->bison symb))))

;; @deffn lalr->bison spec => to current output port
;; needs cleanup: tokens working better but p-rules need fix.
(define (lalr->bison spec . rest)

  (define (setup-assc assc)
    (fold (lambda (al seed)
	    (append (x-flip al) seed)) '() assc))

  (let* ((port (if (pair? rest) (car rest) (current-output-port)))
	 (lhs-v (assq-ref spec 'lhs-v))
	 (rhs-v (assq-ref spec 'rhs-v))
	 (prp-v (assq-ref spec 'prp-v))
	 (assc (setup-assc (assq-ref spec 'assc)))
	 (nrule (vector-length lhs-v))
	 (terms (assq-ref spec 'terminals)))
    ;; Generate copyright notice.
    (let* ((notice (assq-ref (assq-ref spec 'attr) 'notice))
	   (lines (if notice (string-split notice #\newline) '())))
      (for-each (lambda (l) (fmt port "// ~A\n" l))
		lines))
    ;; Write out the tokens.
    (for-each 
     (lambda (term)
       (unless (eqv? term '$error)
	 (fmt port "%token ~A\n" (token->bison term))))
     terms)
    ;; Write the associativity and prececences.
    (let loop ((pl '()) (ppl (assq-ref spec 'prec)))
      (cond
       ((pair? pl)
	(fmt port "%~A" (or (assq-ref assc (caar pl)) "precedence"))
	(let loop2 ((pl (car pl)))
	  (unless (null? pl)
	    (fmt port " ~A" (elt->bison (car pl) terms))
	    (loop2 (cdr pl))))
	(fmt port "\n")
	(loop (cdr pl) ppl))
       ((pair? ppl) (loop (car ppl) (cdr ppl)))))
    ;; Don't compact tables.
    (fmt port "%define lr.default-reduction accepting\n")
    ;; Provide start symbol.
    (fmt port "%start ~A\n%%\n" (elt->bison (lalr-start spec) terms))
    ;;
    (do ((i 1 (1+ i))) ((= i nrule))
      (let* ((lhs (vector-ref lhs-v i)) (rhs (vector-ref rhs-v i)))
	(fmt port "~A:" (elt->bison lhs terms))
	(vector-for-each
	 (lambda (ix e) (fmt port " ~A" (elt->bison e terms)))
	 rhs)
	(if (zero? (vector-length rhs)) (fmt port " %empty"))
	(and=> (vector-ref prp-v i)
	       (lambda (tok) (fmt port " %prec ~A" (elt->bison tok terms))))
	(fmt port " ;\n")))
    (newline port)
    (values)))

;; @item pp-guile-input spec => to current output port
;; total hack right now
(define (lalr->guile spec . rest)
  (let* ((port (if (pair? rest) (car rest) (current-output-port)))
	 (lhs-v (assq-ref spec 'lhs-v))
	 (rhs-v (assq-ref spec 'rhs-v))
	 (act-v (assq-ref spec 'act-v))
	 (nrule (vector-length lhs-v))
	 (terms (assq-ref spec 'terminals))
	 (lhsP #f))
    ;;
    (fmt port "(use-modules (system base lalr))\n")
    (fmt port "(define parser\n")
    (fmt port "  (lalr-parser\n   (")
    (for-each
     (lambda (s)
       (if (> (port-column port) 60) (fmt port "\n    "))
       (cond
	((equal? #\; s) (fmt port " C-semi"))
	((symbol? s) (fmt port " ~A" s))
	(else (fmt port " C-~A" s))))
     terms)
    (fmt port ")\n")
    ;;
    (do ((i 1 (1+ i))) ((= i nrule))
      (let* ((lhs (vector-ref lhs-v i)) (rhs (vector-ref rhs-v i)))
	(if #f
	    (pp-rule 0 i)
	    (begin
	      (if lhsP
		  (if (not (eqv? lhs lhsP))
		      (fmt port "    )\n   (~S\n" lhs))
		  (fmt port "   (~S\n" lhs))
	      (fmt port "    (")
	      (do ((j 0 (1+ j) )) ((= j (vector-length rhs)))
		(let ((e (vector-ref rhs j)))
		  (if (positive? j) (fmt port " "))
		  (fmt
		   port "~A" 
		   (cond
		    ((equal? #\; e) (fmtstr "C-semi"))
		    ((char? e) (fmtstr "C-~A" e))
		    (else e)))))
	      (fmt port ") ")
	      (fmt port ": ~S" `(begin ,@(vector-ref act-v i)))
	      (fmt port "\n")
	      (set! lhsP lhs)))))
    (fmt port "   ))\n")
    (fmt port "  )\n")
    (values)))

;;; --- last line ---

;;; nyacc/import.scm
;;;
;;; Copyright (C) 2015 Matthew R. Wette
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this library; if not, see <http://www.gnu.org/licenses/>

;; Convert guile lalr grammar to nyacc grammar.

;; What is *eoi* for?

(define-module (nyacc import)
  ;;#:export-syntax (lalr-parser)
  #:export (lalr-parser guile-lalr->nyacc-lalr)
  #:use-module ((srfi srfi-1) #:select (fold-right))
  )

(define (convert-tree spec0)
  (let* ((terms (cons '*eoi* (car spec0)))
	 (start (caadr spec0))
	 (wrap-symb
	  (lambda (s) (cons (if (memq s terms) 'terminal 'non-terminal) s))))
    (let loop ((prl1 '())		; new production rules
	       (prl0 (cdr spec0))	; old production rules
	       (lhs #f)			; LHS
	       (rhs1-l #f)		; new RHS list
	       (rhs0-l #f))		; old RHS list
      (cond
       ((pair? rhs0-l) ;; convert RHS
	(loop prl1 prl0 lhs
	      (cons
	       (fold-right ;; s1 ... : a => (('terminal . s) ... ('$$ . a))
		(lambda (symb seed) (cons (wrap-symb symb) seed))
		(list (list '$$ (cdar rhs0-l)))
		(caar rhs0-l))
	       rhs1-l)
	      (cdr rhs0-l)))
       ((null? rhs0-l) ;; roll up LHS+RHSs to new rule
	(loop (cons (cons lhs (reverse rhs1-l)) prl1) prl0 #f #f #f))
       ((pair? prl0) ;; next production rule
	(loop prl1 (cdr prl0) (caar prl0) '() (cdar prl0)))
       (else ;; return spec in preliminary form
	(list
	 'lalr-spec
	 `(start ,start)
	 `(grammar ,(reverse prl1))))))))

(define-syntax parse-rhs-list
  (syntax-rules (:)
    ((_ (<rhs0sym> ...) : <rhs0act> <rhs1> ...)
     (cons (cons '(<rhs0sym> ...) '<rhs0act>)
	   (parse-rhs-list <rhs1> ...)))
    ((_) (list))))

(define-syntax parse-prod-list
  (syntax-rules ()
    ((_ (<lhs> <rhs> ...) <prod1> ...)
     (cons (cons '<lhs> (parse-rhs-list <rhs> ...))
	   (parse-prod-list <prod1> ...)))
    ((_) (list))))


(define-syntax lalr-parser
  (syntax-rules ()
    ((_ <tokens> <prod0> ...)
     (convert-tree
      (cons '<tokens> (parse-prod-list <prod0> ...))))))


(define (guile-lalr->nyacc-lalr match-table spec)
  (letrec
      ((mark (lambda (s) (if (symbol? s) `(quote ,s) s)))
       (rmt (map (lambda (p) (cons (cdr p) (mark (car p)))) match-table))
       (clean
	(lambda (dt)
	  (cond
	   ((null? dt) '())
	   ((pair? dt)
	    (case (car dt)
	      ((non-terminal) (cdr dt))
	      ((terminal)
	       (cond
		((assq-ref rmt (cdr dt)))
		((symbol? (cdr dt)) (simple-format #f "~A" (cdr dt)))
		(else (cdr dt))))
	      ((start) dt)
	      (else
	       (cons (clean (car dt)) (clean (cdr dt))))))
	   (else
	    dt))))
       )
    (clean spec)))


;;; --- last line ---

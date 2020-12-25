;;; -*-scheme-*-

;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright © 2016,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;;
;;; This file is part of GNU Mes.
;;;
;;; GNU Mes is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Mes is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Mes.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; test.mes can be loaded after base.mes.  It provides a minimalistic
;;; test framework: pass-if, pass-if-not, seq?, sequal? and result.

;;; Code:

(define-module (mes test)
  #:use-module (ice-9 rdelim)
  #:export (
            pass-if
            pass-if-equal
            pass-if-not
            pass-if-eq
            pass-if-timeout
            result
            seq? ; deprecated
            sequal? ; deprecated
            ))

(cond-expand
 (mes
  (define (inexact->exact x) x)
  (define mes? #t)
  (define guile? #f)
  (define guile-2? #f)
  (define guile-1.8? #f))
 (guile-2
  (define mes? #f)
  (define guile? #t)
  (define guile-2? #t)
  (define guile-1.8? #f))
 (guile
  (define mes? #f)
  (define guile? #f)
  (define guile-2? #f)
  (define guile-1.8? #t)))

(define result
  ((lambda (pass fail)
     (lambda (. t)
       (if (or (null? t) (eq? (car t) 'result)) (list pass fail)
           (if (eq? (car t) 'report)
               (begin
                 ((lambda (expect)
                    (begin (display "expect: ") (write expect) (newline))
                    (newline)
                    (display "passed: ") (display pass) (newline)
                    (display "failed: ") (display fail) (newline)
                    (if (not (eq? expect 0)) (begin (display "expect: ") (write expect) (newline)))
                    (display "total: ") (display (+ pass fail)) (newline)
                    (exit (if (eq? expect fail) 0 fail)))
                  (if (null? (cdr t)) 0 (cadr t))))
               (if (car t) (begin (display ": pass") (newline) (set! pass (+ pass 1)))
                   (begin (display ": fail") (newline) (set! fail (+ fail 1))))))))
   0 0))

(define (seq? expect a) ;;REMOVE ME
  (or (eq? a expect)
      (begin
        (display ": fail")
        (newline)
        (display "expected: ")
        (write expect) (newline)
        (display "actual: ")
        (write a)
        (newline)
        #f)))

(define (sequal? expect a) ;;REMOVE ME
  (or (equal? a expect)
      (begin
        (display ": fail")
        (newline)
        (display "expected: ")
        (write expect) (newline)
        (display "actual: ")
        (write a)
        (newline)
        #f)))

(define (seq2? a expect)
  (or (eq? a expect)
      (begin
        (display ": fail") (newline)
        (display "expected: ") (write expect) (newline)
        (display "actual: ") (write a) (newline)
        #f)))

(define (sless? a expect)
  (or (< a expect)
      (begin
        (display ": fail") (newline)
        (display "expected: ") (write expect) (newline)
        (display "actual: ") (write a) (newline)
        #f)))

(define (sequal2? actual expect)
  (or (equal? actual expect)
      (begin
        (display ": fail") (newline)
        (display "expected: ") (write expect) (newline)
        (display "actual: ") (write actual) (newline)
        #f)))

(define-macro (pass-if name t)
  (list
   'begin
   (list display "test: ") (list display name)
   (list 'result t))) ;; FIXME

(define-macro (pass-if-eq name expect . body)
  (list 'pass-if name (list seq2? (cons 'begin body) expect)))

(define-macro (pass-if-equal name expect . body)
  (list 'pass-if name (list sequal2? (cons 'begin body) expect)))

(define-macro (expect-fail name expect . body)
  (list 'pass-if name (list not (list sequal2? (cons 'begin body) expect))))

(define-macro (pass-if-not name f)
  (list
   'begin
   (list display "test: ") (list display name)
   (list 'result (list not f)))) ;; FIXME

(define internal-time-units-per-milli-second
  (/ internal-time-units-per-second 1000))
(define (test-time thunk)
  ((lambda (start)
     (begin
       (thunk)
       (inexact->exact (/ (- (get-internal-run-time) start)
                          internal-time-units-per-milli-second))))
   (get-internal-run-time)))

(define-macro (pass-if-timeout name limit . body)
  (list 'pass-if name (list sless? (list test-time (cons* 'lambda '_ body)) limit)))

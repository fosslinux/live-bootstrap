;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(define display core:display)
(define write core:write)
(define (newline) (display "\n"))

(define-macro (or . x)
  (if (null? x) #f
      (if (null? (cdr x)) (car x)
          (list (list (quote lambda) (list (quote r))
                      (list (quote if) (quote r) (quote r)
                            (cons (quote or) (cdr x))))
                (car x)))))

(define (cadr x) (car (cdr x)))
(define (not x) (if x #f #t))

(define result
  ((lambda (pass fail)
     (lambda (. t)
       (if (or (null? t) (eq? (car t) 'result)) (list pass fail)
           (if (eq? (car t) 'report)
               (begin
                 ((lambda (expect)
                    (newline)
                    (display "passed: ") (display pass) (newline)
                    (display "failed: ") (display fail) (newline)
                    (if (not (eq? expect 0)) (begin (display "expect: ") (write expect) (newline)))
                    (display "total: ") (display (+ pass fail)) (newline)
                    (exit (if (eq? expect fail) 0 fail)))
                  (begin
                    (if (null? (cdr t)) 0 (cadr t)))))
               (if (car t) (begin (display ": pass") (newline) (set! pass (+ pass 1)))
                   (begin (display ": fail") (newline) (set! fail (+ fail 1))))))))
   0 0))

(define-macro (pass-if name t)
  (list
   'begin
   (list display "test: ") (list display name)
   (list (quote result) t)))

(pass-if "first dummy" #t)

(result 'report 1)

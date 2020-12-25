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

(define (not x) (if x #f #t))

(define-macro (or . x)
  (if (null? x) #f
      (if (null? (cdr x)) (car x)
          (list (list (quote lambda) (list (quote r))
                      (list (quote if) (quote r) (quote r)
                            (cons (quote or) (cdr x))))
                (car x)))))

(define (boolean? x)
  (or (eq? x #f) (eq? x #t)))

(define (display x . rest)
  (if (null? rest) (core:display x)
      (core:display-port x (car rest))))

(define (write x . rest)
  (if (null? rest) (core:write x)
      (core:write-port x (car rest))))

(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))

;;(define (current-output-port) 1)

(define (simple-format destination format . rest)
  ((lambda (port lst)
     (define (simple-format lst args)
       (if (pair? lst)
           ((lambda (c)
              (if (not (eq? c #\~)) (begin (write-char (car lst) port)
                                           (simple-format (cdr lst) args))
                  ((lambda (c)
                     (if (or (eq? c #\A)
                             (eq? c #\a))
                         (display (car args) port)
                         (if (or (eq? c #\S)
                                 (eq? c #\s))
                             (write (car args) port)
                             (write (car args) port)))
                     (simple-format (cddr lst) (cdr args)))
                   (cadr lst))))
            (car lst))))
     (if destination (simple-format lst rest)
         (with-output-to-string
           (lambda () (simple-format lst rest)))))
   (if (boolean? destination) (current-output-port) destination)
   ;;(string->list format)
   format))
;;(simple-format 2 "~A:~A: parse failed at state ~A, on input ~S\n" "<stdin>" 1 59 "(")
(simple-format #t '(#\~ #\A #\: #\~ #\A #\: #\space #\p #\a #\r #\s #\e #\space #\f #\a #\i #\l #\e #\d #\space #\a #\t #\space #\s #\t #\a #\t #\e #\space #\~ #\A #\, #\space #\o #\n #\space #\i #\n #\p #\u #\t #\space #\~ #\S #\newline) "<stdin>" 1 59 "(")

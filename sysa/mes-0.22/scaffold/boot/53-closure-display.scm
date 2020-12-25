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

(cond-expand
 (guile
  (define closure identity))
 (mes
  (define display core:display)
  (define write core:write)
  (define (newline) (display "\n"))
  (define (cadr x) (car (cdr x)))
  (define (map f lst)
    (if (null? lst) (list)
        (cons (f (car lst)) (map f (cdr lst)))))
  (define (closure x)
    (map car (cdr (core:cdr (core:car (core:cdr (cdr (module-variable (current-module) 'x))))))))))

(define (x t) #t)
(define (xx x1 x2)
  (define blabla 4)
  (define (blubblub) 5)
  #t)

(newline)
(display "x:")
(display x)
(newline)

(newline)
(display "xx:")
(display xx)
(newline)

(display "closure:")
(display closure)
(newline)
(display "closure xx:")
(write (closure xx))
(display "\n")
(xx 0 1)
(display " => closure xx:")
(write (closure xx))
(display "\n")

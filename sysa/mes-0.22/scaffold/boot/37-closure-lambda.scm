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

(define (atom? x)
  (if (pair? x) #f
      (if (null? x) #f
      #t)))

(define (memq x lst)
  (if (null? lst) #f
      (if (eq? x (car lst)) lst
          (memq x (cdr lst)))))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (loop first rest accum)
  (core:display-error "\nloop\n  first=")
  (core:write-error first)
  (core:display-error "\n")
  (core:display-error "  rest=")
  (core:write-error rest)
  (core:display-error "\n")
  (core:display-error "  accum=")
  (core:write-error accum)
  (core:display-error "\n")
  ((lambda (next)
     (if (atom? first)
         (next (cons (cons first
                           (car rest)) accum))
         (if (null? rest)
             accum
             (next accum))))
   (lambda (a)
     (core:display-error "\nnext a=")
     (core:write-error a)
     (core:display-error "\n")
     (core:display-error "     rest=")
     (core:write-error rest)
     (core:display-error "\n")
     (if (null? (cdr rest))
         a
         (loop (cadr rest) (cddr rest) a)))))

(loop 'functions '(() 'globals ()) '())

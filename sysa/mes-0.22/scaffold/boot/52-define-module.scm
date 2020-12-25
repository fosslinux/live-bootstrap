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
  )
 (mes
;;;;;;;;;;;;;;;
  (define (cons* . rest)
    (if (null? (cdr rest)) (car rest)
        (cons (car rest) (core:apply cons* (cdr rest) (current-module)))))

  (define (apply f h . t)
    (if (null? t) (core:apply f h (current-module))
        (apply f (apply cons* (cons h t)))))

  (define (append . rest)
    (if (null? rest) '()
        (if (null? (cdr rest)) (car rest)
            (append2 (car rest) (apply append (cdr rest))))))

  (define-macro (and . x)
    (if (null? x) #t
        (if (null? (cdr x)) (car x)
            (list (quote if) (car x) (cons (quote and) (cdr x))
                  #f))))

  (define (string . lst)
    (list->string lst))

  (define (map1 f lst)
    (if (null? lst) (list)
        (cons (f (car lst)) (map1 f (cdr lst)))))

  (define map map1)

;;;;;;;;;;;;;;;;;;
  (define (string-join lst infix)
    (if (null? (cdr lst)) (car lst)
        (string-append (car lst) infix (string-join (cdr lst) infix))))
;;;;;;;;;;;;;;;;;;

  (define-macro (load file)
    (list 'primitive-load file))

  (define (not x) (if x #f #t))

  (define (memq x lst)
    (if (null? lst) #f
        (if (eq? x (car lst)) lst
            (memq x (cdr lst)))))
  ))

(define %moduledir "./")
(core:display-error "reading...\n")
(primitive-load "mes/module/mes/module.mes")
(core:display-error "dun\n")
(core:write-error (map symbol->string '(scaffold boot data bar)))
(core:display-error "\n")
(core:write-error (string-join (map symbol->string '(scaffold boot data bar)) "/"))
(core:display-error "\n")
(mes-use-module (scaffold boot data bar))

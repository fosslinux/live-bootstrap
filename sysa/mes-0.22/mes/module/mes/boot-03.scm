;;; -*-scheme-*-

;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright © 2016,2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

;;; read-0.mes - bootstrap reader.  This file is read by a minimal
;;; core reader.  It only supports s-exps and line-comments; quotes,
;;; character literals, string literals cannot be used here.

;;; Code:

;; boot-00.scm
(define mes %version)

(define (defined? x)
  (module-variable (current-module) x))

(define (cond-expand-expander clauses)
  (if (defined? (car (car clauses)))
      (cdr (car clauses))
      (cond-expand-expander (cdr clauses))))

(define-macro (cond-expand . clauses)
  (cons 'begin (cond-expand-expander clauses)))
;; end boot-00.scm

;; boot-01.scm
(define (not x) (if x #f #t))

(define (display x . rest)
  (if (null? rest) (core:display x)
      (core:display-port x (car rest))))

(define (write x . rest)
  (if (null? rest) (core:write x)
      (core:write-port x (car rest))))

(define (newline . rest)
  (core:display "\n"))

(define (cadr x) (car (cdr x)))

(define (map1 f lst)
  (if (null? lst) (list)
      (cons (f (car lst)) (map1 f (cdr lst)))))

(define (map f lst)
  (if (null? lst) (list)
      (cons (f (car lst)) (map f (cdr lst)))))

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
;; end boot-01.scm

;; boot-02.scm
(define-macro (and . x)
  (if (null? x) #t
      (if (null? (cdr x)) (car x)
          (list (quote if) (car x) (cons (quote and) (cdr x))
                #f))))

(define-macro (or . x)
  (if (null? x) #f
      (if (null? (cdr x)) (car x)
          (list (list (quote lambda) (list (quote r))
                      (list (quote if) (quote r) (quote r)
                            (cons (quote or) (cdr x))))
                (car x)))))

(define-macro (mes-use-module module)
  #t)
;; end boot-02.scm

;; boot-03.scm
(define guile? #f)
(define mes? #t)
(define (primitive-eval e) (core:eval e (current-module)))
(define eval core:eval)

(define (port-filename port) "<stdin>")
(define (port-line port) 0)
(define (port-column port) 0)
(define (ftell port) 0)
(define (false-if-exception x) x)

(define (cons* . rest)
  (if (null? (cdr rest)) (car rest)
      (cons (car rest) (core:apply cons* (cdr rest) (current-module)))))

(define (apply f h . t)
  (if (null? t) (core:apply f h (current-module))
      (apply f (apply cons* (cons h t)))))

(define-macro (load file)
  (list 'begin
        (list 'if (list 'and (list getenv "MES_DEBUG")
                        (list not (list equal2? (list getenv "MES_DEBUG") "0"))
                        (list not (list equal2? (list getenv "MES_DEBUG") "1")))
              (list 'begin
                    (list core:display-error ";;; read ")
                    (list core:display-error file)
                    (list core:display-error "\n")))
     (list 'primitive-load file)))

(define-macro (include file) (list 'load file))

(define (append . rest)
  (if (null? rest) '()
      (if (null? (cdr rest)) (car rest)
          (append2 (car rest) (apply append (cdr rest))))))

(define %moduledir (string-append %datadir "/module/"))

(include (string-append %moduledir "mes/type-0.mes"))

(if (and (getenv "MES_DEBUG")
          (not (equal2? (getenv "MES_DEBUG") "0"))
          (not (equal2? (getenv "MES_DEBUG") "1")))
    (begin
      (core:display-error ";;; %moduledir=")
      (core:display-error %moduledir)
      (core:display-error "\n")))

(define-macro (include-from-path file)
  (list 'load (list string-append %moduledir file)))

(define (string-join lst infix)
  (if (null? lst) ""
      (if (null? (cdr lst)) (car lst)
          (string-append (car lst) infix (string-join (cdr lst) infix)))))

(include-from-path "mes/module.mes")

(mes-use-module (mes base))
(mes-use-module (mes quasiquote))
(mes-use-module (mes let))
(mes-use-module (mes scm))

(define-macro (define-module module . rest)
  `(if ,(and (pair? module)
             (= 1 (length module))
             (symbol? (car module)))
       (define (,(car module) . arguments) (main (command-line)))))

(define-macro (use-modules . rest) #t)
;; end boot-03.scm
(primitive-load 0)
(primitive-load 0)

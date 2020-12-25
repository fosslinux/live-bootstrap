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
(define <cell:character> 0)
(define <cell:pair> 7)
(define <cell:string> 10)

(define (not x) (if x #f #t))

(define (display x . rest)
  (if (null? rest) (core:display x)
      (core:display-port x (car rest))))

(define (write x . rest)
  (if (null? rest) (core:write x)
      (core:write-port x (car rest))))

(define (integer->char x)
  (core:make-cell <cell:character> 0 x))

(define (newline . rest)
  (core:display (list->string (list (integer->char 10)))))

(define (string->list s)
  (core:car s))

(define (cadr x) (car (cdr x)))

(define (map1 f lst)
  (if (null? lst) (list)
      (cons (f (car lst)) (map1 f (cdr lst)))))

(define map map1)

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

;;((lambda (*program*) *program*) (primitive-load 0))
;;(primitive-load 0)


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

(define else #t)
(define-macro (cond . clauses)
  (list 'if (pair? clauses)
        (list (cons
               'lambda
               (cons
                '(test)
                (list (list 'if 'test
                            (if (pair? (cdr (car clauses)))
                                (if (eq? (car (cdr (car clauses))) '=>)
                                    (append2 (cdr (cdr (car clauses))) '(test))
                                    (list (cons 'lambda (cons '() (cons 'test (cdr (car clauses)))))))
                                (list (cons 'lambda (cons '() (cons 'test (cdr (car clauses)))))))
                            (if (pair? (cdr clauses))
                                (cons 'cond (cdr clauses)))))))
              (car (car clauses)))))

(define (memq x lst)
  (if (null? lst) #f
      (if (eq? x (car lst)) lst
          (memq x (cdr lst)))))

  (define <cell:symbol> 11)
  (define (symbol? x)
    (eq? (core:type x) <cell:symbol>))

  (define <cell:string> 10)
  (define (string? x)
    (eq? (core:type x) <cell:string>))

  (define <cell:vector> 14)
  (define (vector? x)
    (eq? (core:type x) <cell:vector>))

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

(define-macro (quasiquote x)
  (define (loop x)
    (if (vector? x) (list 'list->vector (loop (vector->list x)))
        (if (not (pair? x)) (cons 'quote (cons x '()))
            (if (eq? (car x) 'quasiquote) (loop (loop (cadr x)))
                (if (eq? (car x) 'unquote) (cadr x)
                    (if (and (pair? (car x)) (eq? (caar x) 'unquote-splicing))
                        ((lambda (d)
                           (list 'append (car (cdr (car x))) d))
                         (loop (cdr x)))
                        ((lambda (a d)
                           (if (pair? d)
                               (if (eq? (car d) 'quote)
                                   (if (and (pair? a) (eq? (car a) 'quote))
                                       (list 'quote (cons (cadr a) (cadr d)))
                                       (if (null? (cadr d))
                                           (list 'list a)
                                           (list 'cons* a d)))
                                   (if (memq (car d) '(list cons*))
                                       (cons (car d) (cons a (cdr d)))
                                       (list 'cons* a d)))
                               (list 'cons* a d)))
                         (loop (car x))
                         (loop (cdr x)))))))))
  (loop x))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define-macro (simple-let bindings . rest)
  (cons (cons 'lambda (cons (map car bindings) rest))
        (map cadr bindings)))

(define-macro (xsimple-let bindings rest)
  `(,`(lambda ,(map car bindings) ,@rest)
    ,@(map cadr bindings)))

(define-macro (xnamed-let name bindings rest)
  `(simple-let ((,name *unspecified*))
     (set! ,name (lambda ,(map car bindings) ,@rest))
     (,name ,@(map cadr bindings))))

(define-macro (let bindings-or-name . rest)
  (if (symbol? bindings-or-name) ;; IF
      `(xnamed-let ,bindings-or-name ,(car rest) ,(cdr rest))
      `(xsimple-let ,bindings-or-name ,rest)))

(define (expand-let* bindings body)
  (if (null? bindings)
      `((lambda () ,@body))
      `((lambda (,(caar bindings))
          ,(expand-let* (cdr bindings) body))
        ,@(cdar bindings))))

(define-macro (let* bindings . body)
  (expand-let* bindings body))

(define (equal2? a b)
  (if (and (null? a) (null? b)) #t
      (if (and (pair? a) (pair? b))
          (and (equal2? (car a) (car b))
               (equal2? (cdr a) (cdr b)))
          (if (and (string? a) (string? b))
              (string=? a b)
              (if (and (vector? a) (vector? b))
                  (equal2? (vector->list a) (vector->list b))
                  (eq? a b))))))

(define equal? equal2?)
(define (member x lst)
  (if (null? lst) #f
      (if (equal2? x (car lst)) lst
          (member x (cdr lst)))))

(define (<= . rest)
  (or (apply < rest)
      (apply = rest)))

(define (>= . rest)
  (or (apply > rest)
      (apply = rest)))

(define (list? x)
  (or (null? x)
      (and (pair? x) (list? (cdr x)))))

(cond-expand
 (guile)
 (mes
  (define (boolean? x)
    (or (eq? x #f) (eq? x #t)))
  (define (char? x)
    (and (eq? (core:type x) <cell:char>)
         (> (char->integer x) -1)))))

;; -*-scheme-*-

;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright (c) 1993-2004 by Richard Kelsey and Jonathan Rees.
;;; Copyright © 2016 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

;;; syntax.mes is loaded after scm.mes.  It provides the R5RS hygienic
;;; macros define-syntax, syntax-rules and define-syntax-rule.
;;; syntax-rules is adapted from scheme48-1.1/scheme/alt/syntax.scm

;;; Code:

;;; Copyright (c) 1993-2004 by Richard Kelsey and Jonathan Rees. See file COPYING.

;;; scheme48-1.1/COPYING

;; Copyright (c) 1993-2004 Richard Kelsey and Jonathan Rees
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the authors may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


(cond-expand
 (guile)
 (mes
  (define-macro (define-syntax macro-name transformer . stuff)
    `(define-macro (,macro-name . args)
       (,transformer (cons ',macro-name args)
                     (lambda (x0) x0)
                     eq?)))))

;; Rewrite-rule compiler (a.k.a. "extend-syntax")

;; Example:
;;
;; (define-syntax or
;;   (syntax-rules ()
;;     ((or) #f)
;;     ((or e) e)
;;     ((or e1 e ...) (let ((temp e1))
;;		       (if temp temp (or e ...))))))

(cond-expand
 (guile)
 (mes
  (define-syntax syntax-rules
    (let ()
      (define name? symbol?)

      (define (segment-pattern? pattern)
        (and (segment-template? pattern)
             (or (null? (cddr pattern))
                 (syntax-error0 "segment matching not implemented" pattern))))

      (define (segment-template? pattern)
        (and (pair? pattern)
             (pair? (cdr pattern))
             (memq (cadr pattern) indicators-for-zero-or-more)))

      (define indicators-for-zero-or-more (list (string->symbol "...") '---))

      (lambda (exp r c)

        (define %input (r '%input))     ;Gensym these, if you like.
        (define %compare (r '%compare))
        (define %rename (r '%rename))
        (define %tail (r '%tail))
        (define %temp (r '%temp))

        (define rules (cddr exp))
        (define subkeywords (cadr exp))

        (define (make-transformer rules)
          ;;(core:display-error "make-transformer:") (core:write-error rules) (core:display-error "\n")
          `(lambda (,%input ,%rename ,%compare)
             (let ((,%tail (cdr ,%input)))
               (cond ,@(map process-rule rules)
                     (else
                      (syntax-error1
                       "use of macro doesn't match definition"
                       ,%input))))))

        (define (process-rule rule)
          ;;(core:display-error "process-rule:") (core:write-error rule) (core:display-error "\n")
          (if (and (pair? rule)
                   (pair? (cdr rule))
                   (null? (cddr rule)))
              (let ((pattern (cdar rule))
                    (template (cadr rule)))
                `((and ,@(process-match %tail pattern))
                  (let* ,(process-pattern pattern
                                          %tail
                                          (lambda (x) x))
                    ,(process-template template
                                       0
                                       (meta-variables pattern 0 '())))))
              (syntax-error2 "ill-formed syntax rule" rule)))

        ;; Generate code to test whether input expression matches pattern

        (define (process-match input pattern)
          ;;(core:display-error "process-match:") (core:write-error input) (core:display-error "\n")
          ;;(core:display-error "      pattern:") (core:write-error pattern) (core:display-error "\n")
          (cond ((name? pattern)
                 (if (member pattern subkeywords)
                     `((,%compare ,input (,%rename ',pattern)))
                     `()))
                ((segment-pattern? pattern)
                 (process-segment-match input (car pattern)))
                ((pair? pattern)
                 `((let ((,%temp ,input))
                     (and (pair? ,%temp)
                          ,@(process-match `(car ,%temp) (car pattern))
                          ,@(process-match `(cdr ,%temp) (cdr pattern))))))
                ((or (null? pattern) (boolean? pattern) (char? pattern))
                 `((eq? ,input ',pattern)))
                (else
                 `((equal? ,input ',pattern)))))

        (define (process-segment-match input pattern)
          ;;(core:display-error "process-segment-match:") (core:write-error input) (core:display-error "\n")
          ;;(core:display-error "              pattern:") (core:write-error pattern) (core:display-error "\n")
          (let ((conjuncts (process-match '(car l) pattern)))
            (if (null? conjuncts)
                `((list? ,input))       ;+++
                `((let loop ((l ,input))
                    (or (null? l)
                        (and (pair? l)
                             ,@conjuncts
                             (loop (cdr l)))))))))

        ;; Generate code to take apart the input expression
        ;; This is pretty bad, but it seems to work (can't say why).

        (define (process-pattern pattern path mapit)
          ;;(core:display-error "process-pattern:") (core:write-error pattern) (core:display-error "\n")
          ;;(core:display-error "           path:") (core:write-error path) (core:display-error "\n")
          (cond ((name? pattern)
                 (if (memq pattern subkeywords)
                     '()
                     (list (list pattern (mapit path)))))
                ((segment-pattern? pattern)
                 (process-pattern (car pattern)
                                  %temp
                                  (lambda (x) ;temp is free in x
                                    (mapit (if (eq? %temp x)
                                               path ;+++
                                               `(map (lambda (,%temp) ,x)
                                                     ,path))))))
                ((pair? pattern)
                 (append (process-pattern (car pattern) `(car ,path) mapit)
                         (process-pattern (cdr pattern) `(cdr ,path) mapit)))
                (else '())))

        ;; Generate code to compose the output expression according to template

        (define (process-template template rank env)
          ;;(core:display-error "process-template:") (core:write-error template) (core:display-error "\n")
          (cond ((name? template)
                 (let ((probe (assq template env)))
                   (if probe
                       (if (<= (cdr probe) rank)
                           template
                           (syntax-error3 "template rank error (too few ...'s?)"
                                         template))
                       `(,%rename ',template))))
                ((segment-template? template)
                 (let ((vars
                        (free-meta-variables (car template) (+ rank 1) env '())))
                   (if (null? vars)
                       (silent-syntax-error4 "too many ...'s" template)
                       (let* ((x (process-template (car template)
                                                   (+ rank 1)
                                                   env))
                              (gen (if (equal? (list x) vars)
                                       x ;+++
                                       `(map (lambda ,vars ,x)
                                             ,@vars))))
                         (if (null? (cddr template))
                             gen        ;+++
                             `(append ,gen ,(process-template (cddr template)
                                                              rank env)))))))
                ((pair? template)
                 `(cons ,(process-template (car template) rank env)
                        ,(process-template (cdr template) rank env)))
                (else `(quote ,template))))

        ;; Return an association list of (var . rank)

        (define (meta-variables pattern rank vars)
          ;;(core:display-error "meta-variables:") (core:write-error pattern) (core:display-error "\n")
          (cond ((name? pattern)
                 (if (memq pattern subkeywords)
                     vars
                     (cons (cons pattern rank) vars)))
                ((segment-pattern? pattern)
                 (meta-variables (car pattern) (+ rank 1) vars))
                ((pair? pattern)
                 (meta-variables (car pattern) rank
                                 (meta-variables (cdr pattern) rank vars)))
                (else vars)))

        ;; Return a list of meta-variables of given higher rank

        (define (free-meta-variables template rank env free)
          ;;(core:display-error "meta-variables:") (core:write-error template) (core:display-error "\n")
          (cond ((name? template)
                 (if (and (not (memq template free))
                          (let ((probe (assq template env)))
                            (and probe (>= (cdr probe) rank))))
                     (cons template free)
                     free))
                ((segment-template? template)
                 (free-meta-variables (car template)
                                      rank env
                                      (free-meta-variables (cddr template)
                                                           rank env free)))
                ((pair? template)
                 (free-meta-variables (car template)
                                      rank env
                                      (free-meta-variables (cdr template)
                                                           rank env free)))
                (else free)))

        c                               ;ignored

        ;; Kludge for Scheme48 linker.
        ;; `(cons ,(make-transformer rules)
        ;;          ',(find-free-names-in-syntax-rules subkeywords rules))

        (make-transformer rules))))))

(cond-expand
 (guile)
 (mes
  (define-macro (let-syntax bindings . rest)
    `((lambda ()
        ,@(map (lambda (binding)
                 `(define-macro (,(car binding) . args)
                    (,(cadr binding) (cons ',(car binding) args)
                     (lambda (x0) x0)
                     eq?)))
               bindings)
        ,@rest)))))

(core:display
 (let-syntax ((xwhen (syntax-rules ()
                       ((xwhen condition exp ...)
                        (if (not condition)
                            (begin exp ...))))))
   (xwhen #f 42)))

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


(define-macro (define-syntax macro-name transformer . stuff)
  `(define-macro (,macro-name . args)
     (,transformer (cons ',macro-name args)
                   (lambda (x0) x0)
                   eq?)))

;; Rewrite-rule compiler (a.k.a. "extend-syntax")

;; Example:
;;
;; (define-syntax or
;;   (syntax-rules ()
;;     ((or) #f)
;;     ((or e) e)
;;     ((or e1 e ...) (let ((temp e1))
;;		       (if temp temp (or e ...))))))

(define-syntax syntax-rules
  (let ()
    (define name? symbol?)

    (define (segment-pattern? pattern)
      (and (segment-template? pattern)
           (or (null? (cddr pattern))
               (syntax-error "segment matching not implemented" pattern))))
    
    (define (segment-template? pattern)
      (and (pair? pattern)
           (pair? (cdr pattern))
           (memq (cadr pattern) indicators-for-zero-or-more)))
    
    (define indicators-for-zero-or-more (list (string->symbol "...") '---))
    
    (lambda (exp r c)

      (define %input (r '%input))       ;Gensym these, if you like.
      (define %compare (r '%compare))
      (define %rename (r '%rename))
      (define %tail (r '%tail))
      (define %temp (r '%temp))

      (define rules (cddr exp))
      (define subkeywords (cadr exp))

      (define (make-transformer rules)
        `(lambda (,%input ,%rename ,%compare)
           (let ((,%tail (cdr ,%input)))
             (cond ,@(map process-rule rules)
                   (else
                    (syntax-error
                     "use of macro doesn't match definition"
                     ,%input))))))

      (define (process-rule rule)
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
            (syntax-error "ill-formed syntax rule" rule)))
      
      ;; Generate code to test whether input expression matches pattern

      (define (process-match input pattern)
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
        (let ((conjuncts (process-match '(car l) pattern)))
          (if (null? conjuncts)
              `((list? ,input))			;+++
              `((let loop ((l ,input))
                  (or (null? l)
                      (and (pair? l)
                           ,@conjuncts
                           (loop (cdr l)))))))))
      
      ;; Generate code to take apart the input expression
      ;; This is pretty bad, but it seems to work (can't say why).

      (define (process-pattern pattern path mapit)
        (cond ((name? pattern)
               (if (memq pattern subkeywords)
                   '()
                   (list (list pattern (mapit path)))))
              ((segment-pattern? pattern)
               (process-pattern (car pattern)
                                %temp
                                (lambda (x)	;temp is free in x
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
        (cond ((name? template)
               (let ((probe (assq template env)))
                 (if probe
                     (if (<= (cdr probe) rank)
                         template
                         (syntax-error "template rank error (too few ...'s?)"
                                       template))
                     `(,%rename ',template))))
              ((segment-template? template)
               (let ((vars
                      (free-meta-variables (car template) (+ rank 1) env '())))
                 (if (null? vars)
                     (silent-syntax-error "too many ...'s" template)
                     (let* ((x (process-template (car template)
                                                 (+ rank 1)
                                                 env))
                            (gen (if (equal? (list x) vars)
                                     x ;+++
                                     `(map (lambda ,vars ,x)
                                           ,@vars))))
                       (if (null? (cddr template))
                           gen ;+++
                           `(append ,gen ,(process-template (cddr template)
                                                            rank env)))))))
              ((pair? template)
               `(cons ,(process-template (car template) rank env)
                      ,(process-template (cdr template) rank env)))
              (else `(quote ,template))))

      ;; Return an association list of (var . rank)

      (define (meta-variables pattern rank vars)
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

      c                                 ;ignored

      ;; Kludge for Scheme48 linker.
      ;; `(cons ,(make-transformer rules)
      ;;          ',(find-free-names-in-syntax-rules subkeywords rules))

      (make-transformer rules))))

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

(define (vector? x)
  (eq? (core:type x) <cell:vector>))

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

(define else #t)
(define append append2)
(define (not x) (if x #f #t))

(define-macro (and . x)
  (if (null? x) #t
      (if (null? (cdr x)) (car x)
          (list (quote if) (car x) (cons (quote and) (cdr x))
                #f))))

(define (cons* . rest)
  (if (null? (cdr rest)) (car rest)
      (cons (car rest) (core:apply cons* (cdr rest) (current-module)))))

(define (memq x lst)
  (if (null? lst) #f
      (if (eq? x (car lst)) lst
          (memq x (cdr lst)))))

;; (define (quasiquote-expand x)
;;   (core:display "quasiquote-expand x=") (core:display x) (core:display "\n")
;;   (cond ((null? x)
;;          (core:display "NULL\n")
;;          '())
;;         ((vector? x)
;;          (core:display "vector\n")
;;          (list 'list->vector (quasiquote-expand (vector->list x))))
;;         ((not (pair? x))
;;          (core:display "NOT a pair\n")
;;          (cons 'quote (cons x '())))
;;         ((eq? (car x) 'quasiquote) (quasiquote-expand (quasiquote-expand
;;                                                        (if (null? (cddr x)) (cadr x)
;;                                                            (cons 'list (cdr x))))))
;;         ((eq? (car x) 'unquote) (if (null? (cddr x)) (cadr x)
;;                                     (cons 'list (cdr x))))
;;         ((and (pair? (car x)) (eq? (caar x) 'unquote-splicing))
;;          ((lambda (d)
;;             (if (null? (cddar x)) (list 'append (cadar x) d)
;;                 (list 'quote (append (cdar x) d))))
;;           (quasiquote-expand (cdr x))))
;;         (else
;;          (core:display "ELSje\n")
;;          (core:display "CAR x=") (core:display (car x))
;;          (core:display "\n")
;;          (core:display "CDR x=") (core:display (cdr x))
;;          (core:display "\n")
;;          ((lambda (a d)
;;             (core:display "  a=") (core:display a) (core:display "\n")
;;             (core:display "  d=") (core:display d)

;;             (if (pair? d)
;;                 (if (eq? (car d) 'quote)
;;                     (if (and (pair? a) (eq? (car a) 'quote))
;;                         (list 'quote (cons (cadr a) (cadr d)))
;;                         (if (null? (cadr d))
;;                             (list 'list a)
;;                             (list 'cons* a d)))
;;                     (if (memq (car d) '(list cons*))
;;                         (cons (car d) (cons a (cdr d)))
;;                         (list 'cons* a d)))
;;                 (list 'cons* a d)))
;;           (quasiquote-expand (car x))
;;           (list 'quasiquote-expand (list 'cdr x))))))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (cadar x) (car (cdr (car x))))
(define (cddar x) (cdr (cdr (car x))))

(define (quasiquote-expand x)
  (core:display "quasiquote-expand x=") (core:display x) (core:display "\n")
  (cond ((vector? x) (list 'list->vector (quasiquote-expand (vector->list x))))
        ((not (pair? x)) (cons 'quote (cons x '())))
        ((eq? (car x) 'quasiquote) (quasiquote-expand (quasiquote-expand
                                             (if (null? (cddr x)) (cadr x)
                                                 (cons 'list (cdr x))))))
        ((eq? (car x) 'unquote) (if (null? (cddr x)) (cadr x)
                                    (cons 'list (cdr x))))
        ((and (pair? (car x)) (eq? (caar x) 'unquote-splicing))
         ((lambda (d)
            (if (null? (cddar x)) (list 'append (cadar x) d)
                (list 'quote (append (cdar x) d))))
          (quasiquote-expand (cdr x))))
        (else
         (core:display "ELSje\n")
         (core:display "CAR x=") (core:display (car x))
         (core:display "\n")
         (core:display "CDR x=") (core:display (cdr x))
         (core:display "\n")
         ((lambda (a d)
            (core:display "CAR a=") (core:display a)
            (core:display "\n")
            (core:display "CDR d=") (core:display d)
            (core:display "\n")

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
               (quasiquote-expand (car x))
               (quasiquote-expand (cdr x))
))))

(define-macro (quasiquote x)
  (quasiquote-expand x))

;; (define (remainder x y)
;;   (- x (* (/ x y) y)))
;; (define (even? x)
;;   (eq? 0 (remainder x v2)))
;; (pass-if-equal "qq 4" '#(10 5 #t #t #f #f #f 8)
;;                `#(10 5 ,(even? 4) ,@(map even? '(2 3 5 7)) 8))
;;(core:display (quasiquote #(42)))
(core:display (quasiquote-expand #(42)))

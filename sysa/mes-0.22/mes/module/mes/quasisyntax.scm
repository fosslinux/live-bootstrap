;; Quasisyntax in terms of syntax-case.
;;
;; Code taken from
;; <http://www.het.brown.edu/people/andre/macros/index.html>;
;; Copyright (c) 2006 Andre van Tonder. All Rights Reserved.
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;=========================================================
;;
;; To make nested unquote-splicing behave in a useful way,
;; the R5RS-compatible extension of quasiquote in appendix B
;; of the following paper is here ported to quasisyntax:
;;
;; Alan Bawden - Quasiquotation in Lisp
;; http://citeseer.ist.psu.edu/bawden99quasiquotation.html
;;
;; The algorithm converts a quasisyntax expression to an
;; equivalent with-syntax expression.
;; For example:
;;
;; (quasisyntax (set! #,a #,b))
;;   ==> (with-syntax ((t0 a)
;;                     (t1 b))
;;         (syntax (set! t0 t1)))
;;
;; (quasisyntax (list #,@args))
;;   ==> (with-syntax (((t ...) args))
;;         (syntax (list t ...)))
;;
;; Note that quasisyntax is expanded first, before any
;; ellipses act.  For example:
;;
;; (quasisyntax (f ((b #,a) ...))
;;   ==> (with-syntax ((t a))
;;         (syntax (f ((b t) ...))))
;;
;; so that
;;
;; (let-syntax ((test-ellipses-over-unsyntax
;;               (lambda (e)
;;                 (let ((a (syntax a)))
;;                   (with-syntax (((b ...) (syntax (1 2 3))))
;;                     (quasisyntax
;;                      (quote ((b #,a) ...))))))))
;;   (test-ellipses-over-unsyntax))
;;
;;     ==> ((1 a) (2 a) (3 a))
(define-syntax quasisyntax
  (lambda (e)

    ;; Expand returns a list of the form
    ;;    [template[t/e, ...] (replacement ...)]
    ;; Here template[t/e ...] denotes the original template
    ;; with unquoted expressions e replaced by fresh
    ;; variables t, followed by the appropriate ellipses
    ;; if e is also spliced.
    ;; The second part of the return value is the list of
    ;; replacements, each of the form (t e) if e is just
    ;; unquoted, or ((t ...) e) if e is also spliced.
    ;; This will be the list of bindings of the resulting
    ;; with-syntax expression.

    (define (expand x level)
      (syntax-case x (quasisyntax unsyntax unsyntax-splicing)
        ((quasisyntax e)
         (with-syntax (((k _)     x) ;; original identifier must be copied
                       ((e* reps) (expand (syntax e) (+ level 1))))
           (syntax ((k e*) reps))))
        ((unsyntax e)
         (= level 0)
         (with-syntax (((t) (generate-temporaries '(t))))
           (syntax (t ((t e))))))
        (((unsyntax e ...) . r)
         (= level 0)
         (with-syntax (((r* (rep ...)) (expand (syntax r) 0))
                       ((t ...)        (generate-temporaries (syntax (e ...)))))
           (syntax ((t ... . r*)
                    ((t e) ... rep ...)))))
        (((unsyntax-splicing e ...) . r)
         (= level 0)
         (with-syntax (((r* (rep ...)) (expand (syntax r) 0))
                       ((t ...)        (generate-temporaries (syntax (e ...)))))
           (with-syntax ((((t ...) ...) (syntax ((t (... ...)) ...))))
             (syntax ((t ... ... . r*)
                      (((t ...) e) ... rep ...))))))
        ((k . r)
         (and (> level 0)
              (identifier? (syntax k))
              (or (free-identifier=? (syntax k) (syntax unsyntax))
                  (free-identifier=? (syntax k) (syntax unsyntax-splicing))))
         (with-syntax (((r* reps) (expand (syntax r) (- level 1))))
           (syntax ((k . r*) reps))))
        ((h . t)
         (with-syntax (((h* (rep1 ...)) (expand (syntax h) level))
                       ((t* (rep2 ...)) (expand (syntax t) level)))
           (syntax ((h* . t*)
                    (rep1 ... rep2 ...)))))
        (#(e ...)
         (with-syntax ((((e* ...) reps)
                        (expand (vector->list (syntax #(e ...))) level)))
           (syntax (#(e* ...) reps))))
        (other
         (syntax (other ())))))

    (syntax-case e ()
      ((_ template)
       (with-syntax (((template* replacements) (expand (syntax template) 0)))
         (syntax
          (with-syntax replacements (syntax template*))))))))

(define-syntax unsyntax
  (lambda (e)
    (syntax-violation 'unsyntax "Invalid expression" e)))

(define-syntax unsyntax-splicing
  (lambda (e)
    (syntax-violation 'unsyntax "Invalid expression" e)))

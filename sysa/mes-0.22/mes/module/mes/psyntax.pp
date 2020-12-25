;;; -*-scheme-*-
;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright (C) 2016, 2017, 2018 Free Software Foundation, Inc.
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

;;; This file is generated from psyntax.ss.

;;; Code:
(letrec ((syntmp-lambda-var-list-167
           (lambda (syntmp-vars-552)
             (let syntmp-lvl-553 ((syntmp-vars-554 syntmp-vars-552)
                                  (syntmp-ls-555 (quote ()))
                                  (syntmp-w-556 (quote (()))))
               (cond ((pair? syntmp-vars-554)
                      (syntmp-lvl-553
                        (cdr syntmp-vars-554)
                        (cons (syntmp-wrap-146
                                (car syntmp-vars-554)
                                syntmp-w-556)
                              syntmp-ls-555)
                        syntmp-w-556))
                     ((syntmp-id?-118 syntmp-vars-554)
                      (cons (syntmp-wrap-146 syntmp-vars-554 syntmp-w-556)
                            syntmp-ls-555))
                     ((null? syntmp-vars-554) syntmp-ls-555)
                     ((syntmp-syntax-object?-104 syntmp-vars-554)
                      (syntmp-lvl-553
                        (syntmp-syntax-object-expression-105
                          syntmp-vars-554)
                        syntmp-ls-555
                        (syntmp-join-wraps-137
                          syntmp-w-556
                          (syntmp-syntax-object-wrap-106 syntmp-vars-554))))
                     ((syntmp-annotation?-92 syntmp-vars-554)
                      (syntmp-lvl-553
                        (annotation-expression syntmp-vars-554)
                        syntmp-ls-555
                        syntmp-w-556))
                     (else (cons syntmp-vars-554 syntmp-ls-555))))))
         (syntmp-gen-var-166
           (lambda (syntmp-id-557)
             (let ((syntmp-id-558
                     (if (syntmp-syntax-object?-104 syntmp-id-557)
                       (syntmp-syntax-object-expression-105
                         syntmp-id-557)
                       syntmp-id-557)))
               (if (syntmp-annotation?-92 syntmp-id-558)
                 (gensym
                   (symbol->string
                     (annotation-expression syntmp-id-558)))
                 (gensym (symbol->string syntmp-id-558))))))
         (syntmp-strip-165
           (lambda (syntmp-x-559 syntmp-w-560)
             (if (memq 'top
                       (syntmp-wrap-marks-121 syntmp-w-560))
               (if (or (syntmp-annotation?-92 syntmp-x-559)
                       (and (pair? syntmp-x-559)
                            (syntmp-annotation?-92 (car syntmp-x-559))))
                 (syntmp-strip-annotation-164 syntmp-x-559 #f)
                 syntmp-x-559)
               (let syntmp-f-561 ((syntmp-x-562 syntmp-x-559))
                 (cond ((syntmp-syntax-object?-104 syntmp-x-562)
                        (syntmp-strip-165
                          (syntmp-syntax-object-expression-105
                            syntmp-x-562)
                          (syntmp-syntax-object-wrap-106 syntmp-x-562)))
                       ((pair? syntmp-x-562)
                        (let ((syntmp-a-563 (syntmp-f-561 (car syntmp-x-562)))
                              (syntmp-d-564 (syntmp-f-561 (cdr syntmp-x-562))))
                          (if (and (eq? syntmp-a-563 (car syntmp-x-562))
                                   (eq? syntmp-d-564 (cdr syntmp-x-562)))
                            syntmp-x-562
                            (cons syntmp-a-563 syntmp-d-564))))
                       ((vector? syntmp-x-562)
                        (let ((syntmp-old-565 (vector->list syntmp-x-562)))
                          (let ((syntmp-new-566
                                  (map syntmp-f-561 syntmp-old-565)))
                            (if (andmap eq? syntmp-old-565 syntmp-new-566)
                              syntmp-x-562
                              (list->vector syntmp-new-566)))))
                       (else syntmp-x-562))))))
         (syntmp-strip-annotation-164
           (lambda (syntmp-x-567 syntmp-parent-568)
             (cond ((pair? syntmp-x-567)
                    (let ((syntmp-new-569 (cons #f #f)))
                      (begin
                        (when syntmp-parent-568
                              (set-annotation-stripped!
                                syntmp-parent-568
                                syntmp-new-569))
                        (set-car!
                          syntmp-new-569
                          (syntmp-strip-annotation-164
                            (car syntmp-x-567)
                            #f))
                        (set-cdr!
                          syntmp-new-569
                          (syntmp-strip-annotation-164
                            (cdr syntmp-x-567)
                            #f))
                        syntmp-new-569)))
                   ((syntmp-annotation?-92 syntmp-x-567)
                    (or (annotation-stripped syntmp-x-567)
                        (syntmp-strip-annotation-164
                          (annotation-expression syntmp-x-567)
                          syntmp-x-567)))
                   ((vector? syntmp-x-567)
                    (let ((syntmp-new-570
                            (make-vector (vector-length syntmp-x-567))))
                      (begin
                        (when syntmp-parent-568
                              (set-annotation-stripped!
                                syntmp-parent-568
                                syntmp-new-570))
                        (let syntmp-loop-571 ((syntmp-i-572
                                                (- (vector-length syntmp-x-567)
                                                   1)))
                          (unless
                            (syntmp-fx<-91 syntmp-i-572 0)
                            (vector-set!
                              syntmp-new-570
                              syntmp-i-572
                              (syntmp-strip-annotation-164
                                (vector-ref syntmp-x-567 syntmp-i-572)
                                #f))
                            (syntmp-loop-571 (syntmp-fx--89 syntmp-i-572 1))))
                        syntmp-new-570)))
                   (else syntmp-x-567))))
         (syntmp-ellipsis?-163
           (lambda (syntmp-e-573 syntmp-r-574)
             (and (syntmp-nonsymbol-id?-117 syntmp-e-573)
                  (let ((syntmp-id-575
                          (syntmp-make-syntax-object-103
                            '$sc-ellipsis
                            (syntmp-syntax-object-wrap-106 syntmp-e-573))))
                    (let ((syntmp-n-576
                            (syntmp-id-var-name-140
                              syntmp-id-575
                              '(()))))
                      (let ((syntmp-b-577
                              (syntmp-lookup-115 syntmp-n-576 syntmp-r-574)))
                        (if (eq? (syntmp-binding-type-110 syntmp-b-577)
                                 'ellipsis)
                          (syntmp-bound-id=?-142
                            syntmp-e-573
                            (syntmp-binding-value-111 syntmp-b-577))
                          (syntmp-free-id=?-141
                            syntmp-e-573
                            '#(syntax-object
                               ...
                               ((top)
                                #(ribcage () () ())
                                #(ribcage () () ())
                                #(ribcage #(b) #((top)) #("i"))
                                #(ribcage () () ())
                                #(ribcage #(n) #((top)) #("i"))
                                #(ribcage () () ())
                                #(ribcage #(id) #((top)) #("i"))
                                #(ribcage () () ())
                                #(ribcage #(e r) #((top) (top)) #("i" "i"))
                                #(ribcage
                                  (lambda-var-list
                                    gen-var
                                    strip
                                    strip-annotation
                                    ellipsis?
                                    chi-void
                                    eval-local-transformer
                                    chi-local-syntax
                                    chi-lambda-clause
                                    chi-body
                                    chi-macro
                                    chi-application
                                    chi-expr
                                    chi
                                    chi-top
                                    syntax-type
                                    chi-when-list
                                    chi-install-global
                                    chi-top-sequence
                                    chi-sequence
                                    source-wrap
                                    wrap
                                    bound-id-member?
                                    distinct-bound-ids?
                                    valid-bound-ids?
                                    bound-id=?
                                    free-id=?
                                    id-var-name
                                    same-marks?
                                    join-marks
                                    join-wraps
                                    smart-append
                                    make-binding-wrap
                                    extend-ribcage!
                                    make-empty-ribcage
                                    new-mark
                                    anti-mark
                                    the-anti-mark
                                    top-marked?
                                    top-wrap
                                    empty-wrap
                                    set-ribcage-labels!
                                    set-ribcage-marks!
                                    set-ribcage-symnames!
                                    ribcage-labels
                                    ribcage-marks
                                    ribcage-symnames
                                    ribcage?
                                    make-ribcage
                                    gen-labels
                                    gen-label
                                    make-rename
                                    rename-marks
                                    rename-new
                                    rename-old
                                    subst-rename?
                                    wrap-subst
                                    wrap-marks
                                    make-wrap
                                    id-sym-name&marks
                                    id-sym-name
                                    id?
                                    nonsymbol-id?
                                    global-extend
                                    lookup
                                    macros-only-env
                                    extend-var-env
                                    extend-env
                                    null-env
                                    binding-value
                                    binding-type
                                    make-binding
                                    arg-check
                                    source-annotation
                                    no-source
                                    unannotate
                                    set-syntax-object-wrap!
                                    set-syntax-object-expression!
                                    syntax-object-wrap
                                    syntax-object-expression
                                    syntax-object?
                                    make-syntax-object
                                    build-lexical-var
                                    build-letrec
                                    build-named-let
                                    build-let
                                    build-sequence
                                    build-data
                                    build-primref
                                    build-lambda
                                    build-global-definition
                                    build-global-assignment
                                    build-global-reference
                                    build-lexical-assignment
                                    build-lexical-reference
                                    build-conditional
                                    build-application
                                    get-global-definition-hook
                                    put-global-definition-hook
                                    gensym-hook
                                    error-hook
                                    local-eval-hook
                                    top-level-eval-hook
                                    annotation?
                                    fx<
                                    fx=
                                    fx-
                                    fx+
                                    noexpand)
                                  ((top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top))
                                  ("i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"
                                   "i"))
                                #(ribcage
                                  (define-structure)
                                  ((top))
                                  ("i"))))))))))))
         (syntmp-chi-void-162
           (lambda () (list (quote void))))
         (syntmp-eval-local-transformer-161
           (lambda (syntmp-expanded-578)
             (let ((syntmp-p-579
                     (syntmp-local-eval-hook-94 syntmp-expanded-578)))
               (if (procedure? syntmp-p-579)
                 syntmp-p-579
                 (syntax-error
                   syntmp-p-579
                   "nonprocedure transformer")))))
         (syntmp-chi-local-syntax-160
           (lambda (syntmp-rec?-580
                    syntmp-e-581
                    syntmp-r-582
                    syntmp-w-583
                    syntmp-s-584
                    syntmp-k-585)
             ((lambda (syntmp-tmp-586)
                ((lambda (syntmp-tmp-587)
                   (if syntmp-tmp-587
                     (apply (lambda (syntmp-_-588
                                     syntmp-id-589
                                     syntmp-val-590
                                     syntmp-e1-591
                                     syntmp-e2-592)
                              (let ((syntmp-ids-593 syntmp-id-589))
                                (if (not (syntmp-valid-bound-ids?-143
                                           syntmp-ids-593))
                                  (syntax-error
                                    syntmp-e-581
                                    "duplicate bound keyword in")
                                  (let ((syntmp-labels-595
                                          (syntmp-gen-labels-124
                                            syntmp-ids-593)))
                                    (let ((syntmp-new-w-596
                                            (syntmp-make-binding-wrap-135
                                              syntmp-ids-593
                                              syntmp-labels-595
                                              syntmp-w-583)))
                                      (syntmp-k-585
                                        (cons syntmp-e1-591 syntmp-e2-592)
                                        (syntmp-extend-env-112
                                          syntmp-labels-595
                                          (let ((syntmp-w-598
                                                  (if syntmp-rec?-580
                                                    syntmp-new-w-596
                                                    syntmp-w-583))
                                                (syntmp-trans-r-599
                                                  (syntmp-macros-only-env-114
                                                    syntmp-r-582)))
                                            (map (lambda (syntmp-x-600)
                                                   (cons 'macro
                                                         (syntmp-eval-local-transformer-161
                                                           (syntmp-chi-154
                                                             syntmp-x-600
                                                             syntmp-trans-r-599
                                                             syntmp-w-598))))
                                                 syntmp-val-590))
                                          syntmp-r-582)
                                        syntmp-new-w-596
                                        syntmp-s-584))))))
                            syntmp-tmp-587)
                     ((lambda (syntmp-_-602)
                        (syntax-error
                          (syntmp-source-wrap-147
                            syntmp-e-581
                            syntmp-w-583
                            syntmp-s-584)))
                      syntmp-tmp-586)))
                 (syntax-dispatch
                   syntmp-tmp-586
                   '(any #(each (any any)) any . each-any))))
              syntmp-e-581)))
         (syntmp-chi-lambda-clause-159
           (lambda (syntmp-e-603
                    syntmp-c-604
                    syntmp-r-605
                    syntmp-w-606
                    syntmp-k-607)
             ((lambda (syntmp-tmp-608)
                ((lambda (syntmp-tmp-609)
                   (if syntmp-tmp-609
                     (apply (lambda (syntmp-id-610 syntmp-e1-611 syntmp-e2-612)
                              (let ((syntmp-ids-613 syntmp-id-610))
                                (if (not (syntmp-valid-bound-ids?-143
                                           syntmp-ids-613))
                                  (syntax-error
                                    syntmp-e-603
                                    "invalid parameter list in")
                                  (let ((syntmp-labels-615
                                          (syntmp-gen-labels-124
                                            syntmp-ids-613))
                                        (syntmp-new-vars-616
                                          (map syntmp-gen-var-166
                                               syntmp-ids-613)))
                                    (syntmp-k-607
                                      syntmp-new-vars-616
                                      (syntmp-chi-body-158
                                        (cons syntmp-e1-611 syntmp-e2-612)
                                        syntmp-e-603
                                        (syntmp-extend-var-env-113
                                          syntmp-labels-615
                                          syntmp-new-vars-616
                                          syntmp-r-605)
                                        (syntmp-make-binding-wrap-135
                                          syntmp-ids-613
                                          syntmp-labels-615
                                          syntmp-w-606)))))))
                            syntmp-tmp-609)
                     ((lambda (syntmp-tmp-618)
                        (if syntmp-tmp-618
                          (apply (lambda (syntmp-ids-619
                                          syntmp-e1-620
                                          syntmp-e2-621)
                                   (let ((syntmp-old-ids-622
                                           (syntmp-lambda-var-list-167
                                             syntmp-ids-619)))
                                     (if (not (syntmp-valid-bound-ids?-143
                                                syntmp-old-ids-622))
                                       (syntax-error
                                         syntmp-e-603
                                         "invalid parameter list in")
                                       (let ((syntmp-labels-623
                                               (syntmp-gen-labels-124
                                                 syntmp-old-ids-622))
                                             (syntmp-new-vars-624
                                               (map syntmp-gen-var-166
                                                    syntmp-old-ids-622)))
                                         (syntmp-k-607
                                           (let syntmp-f-625 ((syntmp-ls1-626
                                                                (cdr syntmp-new-vars-624))
                                                              (syntmp-ls2-627
                                                                (car syntmp-new-vars-624)))
                                             (if (null? syntmp-ls1-626)
                                               syntmp-ls2-627
                                               (syntmp-f-625
                                                 (cdr syntmp-ls1-626)
                                                 (cons (car syntmp-ls1-626)
                                                       syntmp-ls2-627))))
                                           (syntmp-chi-body-158
                                             (cons syntmp-e1-620 syntmp-e2-621)
                                             syntmp-e-603
                                             (syntmp-extend-var-env-113
                                               syntmp-labels-623
                                               syntmp-new-vars-624
                                               syntmp-r-605)
                                             (syntmp-make-binding-wrap-135
                                               syntmp-old-ids-622
                                               syntmp-labels-623
                                               syntmp-w-606)))))))
                                 syntmp-tmp-618)
                          ((lambda (syntmp-_-629)
                             (syntax-error syntmp-e-603))
                           syntmp-tmp-608)))
                      (syntax-dispatch
                        syntmp-tmp-608
                        '(any any . each-any)))))
                 (syntax-dispatch
                   syntmp-tmp-608
                   '(each-any any . each-any))))
              syntmp-c-604)))
         (syntmp-chi-body-158
           (lambda (syntmp-body-630
                    syntmp-outer-form-631
                    syntmp-r-632
                    syntmp-w-633)
             (let ((syntmp-r-634
                     (cons '("placeholder" placeholder)
                           syntmp-r-632)))
               (let ((syntmp-ribcage-635
                       (syntmp-make-ribcage-125
                         '()
                         '()
                         '())))
                 (let ((syntmp-w-636
                         (syntmp-make-wrap-120
                           (syntmp-wrap-marks-121 syntmp-w-633)
                           (cons syntmp-ribcage-635
                                 (syntmp-wrap-subst-122 syntmp-w-633)))))
                   (let syntmp-parse-637 ((syntmp-body-638
                                            (map (lambda (syntmp-x-644)
                                                   (cons syntmp-r-634
                                                         (syntmp-wrap-146
                                                           syntmp-x-644
                                                           syntmp-w-636)))
                                                 syntmp-body-630))
                                          (syntmp-ids-639 (quote ()))
                                          (syntmp-labels-640 (quote ()))
                                          (syntmp-vars-641 (quote ()))
                                          (syntmp-vals-642 (quote ()))
                                          (syntmp-bindings-643 (quote ())))
                     (if (null? syntmp-body-638)
                       (syntax-error
                         syntmp-outer-form-631
                         "no expressions in body")
                       (let ((syntmp-e-645 (cdar syntmp-body-638))
                             (syntmp-er-646 (caar syntmp-body-638)))
                         (call-with-values
                           (lambda ()
                             (syntmp-syntax-type-152
                               syntmp-e-645
                               syntmp-er-646
                               '(())
                               #f
                               syntmp-ribcage-635))
                           (lambda (syntmp-type-647
                                    syntmp-value-648
                                    syntmp-e-649
                                    syntmp-w-650
                                    syntmp-s-651)
                             (let ((syntmp-t-652 syntmp-type-647))
                               (if (memv syntmp-t-652 (quote (define-form)))
                                 (let ((syntmp-id-653
                                         (syntmp-wrap-146
                                           syntmp-value-648
                                           syntmp-w-650))
                                       (syntmp-label-654
                                         (syntmp-gen-label-123)))
                                   (let ((syntmp-var-655
                                           (syntmp-gen-var-166 syntmp-id-653)))
                                     (begin
                                       (syntmp-extend-ribcage!-134
                                         syntmp-ribcage-635
                                         syntmp-id-653
                                         syntmp-label-654)
                                       (syntmp-parse-637
                                         (cdr syntmp-body-638)
                                         (cons syntmp-id-653 syntmp-ids-639)
                                         (cons syntmp-label-654
                                               syntmp-labels-640)
                                         (cons syntmp-var-655 syntmp-vars-641)
                                         (cons (cons syntmp-er-646
                                                     (syntmp-wrap-146
                                                       syntmp-e-649
                                                       syntmp-w-650))
                                               syntmp-vals-642)
                                         (cons (cons 'lexical
                                                     syntmp-var-655)
                                               syntmp-bindings-643)))))
                                 (if (memv syntmp-t-652
                                           '(define-syntax-form))
                                   (let ((syntmp-id-656
                                           (syntmp-wrap-146
                                             syntmp-value-648
                                             syntmp-w-650))
                                         (syntmp-label-657
                                           (syntmp-gen-label-123)))
                                     (begin
                                       (syntmp-extend-ribcage!-134
                                         syntmp-ribcage-635
                                         syntmp-id-656
                                         syntmp-label-657)
                                       (syntmp-parse-637
                                         (cdr syntmp-body-638)
                                         (cons syntmp-id-656 syntmp-ids-639)
                                         (cons syntmp-label-657
                                               syntmp-labels-640)
                                         syntmp-vars-641
                                         syntmp-vals-642
                                         (cons (cons 'macro
                                                     (cons syntmp-er-646
                                                           (syntmp-wrap-146
                                                             syntmp-e-649
                                                             syntmp-w-650)))
                                               syntmp-bindings-643))))
                                   (if (memv syntmp-t-652 (quote (begin-form)))
                                     ((lambda (syntmp-tmp-658)
                                        ((lambda (syntmp-tmp-659)
                                           (if syntmp-tmp-659
                                             (apply (lambda (syntmp-_-660
                                                             syntmp-e1-661)
                                                      (syntmp-parse-637
                                                        (let syntmp-f-662 ((syntmp-forms-663
                                                                             syntmp-e1-661))
                                                          (if (null? syntmp-forms-663)
                                                            (cdr syntmp-body-638)
                                                            (cons (cons syntmp-er-646
                                                                        (syntmp-wrap-146
                                                                          (car syntmp-forms-663)
                                                                          syntmp-w-650))
                                                                  (syntmp-f-662
                                                                    (cdr syntmp-forms-663)))))
                                                        syntmp-ids-639
                                                        syntmp-labels-640
                                                        syntmp-vars-641
                                                        syntmp-vals-642
                                                        syntmp-bindings-643))
                                                    syntmp-tmp-659)
                                             (syntax-error syntmp-tmp-658)))
                                         (syntax-dispatch
                                           syntmp-tmp-658
                                           '(any . each-any))))
                                      syntmp-e-649)
                                     (if (memv syntmp-t-652
                                               '(local-syntax-form))
                                       (syntmp-chi-local-syntax-160
                                         syntmp-value-648
                                         syntmp-e-649
                                         syntmp-er-646
                                         syntmp-w-650
                                         syntmp-s-651
                                         (lambda (syntmp-forms-665
                                                  syntmp-er-666
                                                  syntmp-w-667
                                                  syntmp-s-668)
                                           (syntmp-parse-637
                                             (let syntmp-f-669 ((syntmp-forms-670
                                                                  syntmp-forms-665))
                                               (if (null? syntmp-forms-670)
                                                 (cdr syntmp-body-638)
                                                 (cons (cons syntmp-er-666
                                                             (syntmp-wrap-146
                                                               (car syntmp-forms-670)
                                                               syntmp-w-667))
                                                       (syntmp-f-669
                                                         (cdr syntmp-forms-670)))))
                                             syntmp-ids-639
                                             syntmp-labels-640
                                             syntmp-vars-641
                                             syntmp-vals-642
                                             syntmp-bindings-643)))
                                       (if (null? syntmp-ids-639)
                                         (syntmp-build-sequence-99
                                           #f
                                           (map (lambda (syntmp-x-671)
                                                  (syntmp-chi-154
                                                    (cdr syntmp-x-671)
                                                    (car syntmp-x-671)
                                                    '(())))
                                                (cons (cons syntmp-er-646
                                                            (syntmp-source-wrap-147
                                                              syntmp-e-649
                                                              syntmp-w-650
                                                              syntmp-s-651))
                                                      (cdr syntmp-body-638))))
                                         (begin
                                           (if (not (syntmp-valid-bound-ids?-143
                                                      syntmp-ids-639))
                                             (syntax-error
                                               syntmp-outer-form-631
                                               "invalid or duplicate identifier in definition"))
                                           (let syntmp-loop-672 ((syntmp-bs-673
                                                                   syntmp-bindings-643)
                                                                 (syntmp-er-cache-674
                                                                   #f)
                                                                 (syntmp-r-cache-675
                                                                   #f))
                                             (if (not (null? syntmp-bs-673))
                                               (let ((syntmp-b-676
                                                       (car syntmp-bs-673)))
                                                 (if (eq? (car syntmp-b-676)
                                                          'macro)
                                                   (let ((syntmp-er-677
                                                           (cadr syntmp-b-676)))
                                                     (let ((syntmp-r-cache-678
                                                             (if (eq? syntmp-er-677
                                                                      syntmp-er-cache-674)
                                                               syntmp-r-cache-675
                                                               (syntmp-macros-only-env-114
                                                                 syntmp-er-677))))
                                                       (begin
                                                         (set-cdr!
                                                           syntmp-b-676
                                                           (syntmp-eval-local-transformer-161
                                                             (syntmp-chi-154
                                                               (cddr syntmp-b-676)
                                                               syntmp-r-cache-678
                                                               '(()))))
                                                         (syntmp-loop-672
                                                           (cdr syntmp-bs-673)
                                                           syntmp-er-677
                                                           syntmp-r-cache-678))))
                                                   (syntmp-loop-672
                                                     (cdr syntmp-bs-673)
                                                     syntmp-er-cache-674
                                                     syntmp-r-cache-675)))))
                                           (set-cdr!
                                             syntmp-r-634
                                             (syntmp-extend-env-112
                                               syntmp-labels-640
                                               syntmp-bindings-643
                                               (cdr syntmp-r-634)))
                                           (syntmp-build-letrec-102
                                             #f
                                             syntmp-vars-641
                                             (map (lambda (syntmp-x-679)
                                                    (syntmp-chi-154
                                                      (cdr syntmp-x-679)
                                                      (car syntmp-x-679)
                                                      '(())))
                                                  syntmp-vals-642)
                                             (syntmp-build-sequence-99
                                               #f
                                               (map (lambda (syntmp-x-680)
                                                      (syntmp-chi-154
                                                        (cdr syntmp-x-680)
                                                        (car syntmp-x-680)
                                                        '(())))
                                                    (cons (cons syntmp-er-646
                                                                (syntmp-source-wrap-147
                                                                  syntmp-e-649
                                                                  syntmp-w-650
                                                                  syntmp-s-651))
                                                          (cdr syntmp-body-638))))))))))))))))))))))
         (syntmp-chi-macro-157
           (lambda (syntmp-p-681
                    syntmp-e-682
                    syntmp-r-683
                    syntmp-w-684
                    syntmp-rib-685)
             (letrec ((syntmp-rebuild-macro-output-686
                        (lambda (syntmp-x-687 syntmp-m-688)
                          (cond ((pair? syntmp-x-687)
                                 (cons (syntmp-rebuild-macro-output-686
                                         (car syntmp-x-687)
                                         syntmp-m-688)
                                       (syntmp-rebuild-macro-output-686
                                         (cdr syntmp-x-687)
                                         syntmp-m-688)))
                                ((syntmp-syntax-object?-104 syntmp-x-687)
                                 (let ((syntmp-w-689
                                         (syntmp-syntax-object-wrap-106
                                           syntmp-x-687)))
                                   (let ((syntmp-ms-690
                                           (syntmp-wrap-marks-121
                                             syntmp-w-689))
                                         (syntmp-s-691
                                           (syntmp-wrap-subst-122
                                             syntmp-w-689)))
                                     (syntmp-make-syntax-object-103
                                       (syntmp-syntax-object-expression-105
                                         syntmp-x-687)
                                       (if (and (pair? syntmp-ms-690)
                                                (eq? (car syntmp-ms-690) #f))
                                         (syntmp-make-wrap-120
                                           (cdr syntmp-ms-690)
                                           (if syntmp-rib-685
                                             (cons syntmp-rib-685
                                                   (cdr syntmp-s-691))
                                             (cdr syntmp-s-691)))
                                         (syntmp-make-wrap-120
                                           (cons syntmp-m-688 syntmp-ms-690)
                                           (if syntmp-rib-685
                                             (cons syntmp-rib-685
                                                   (cons 'shift
                                                         syntmp-s-691))
                                             (cons 'shift
                                                   syntmp-s-691))))))))
                                ((vector? syntmp-x-687)
                                 (let ((syntmp-n-692
                                         (vector-length syntmp-x-687)))
                                   (let ((syntmp-v-693
                                           (make-vector syntmp-n-692)))
                                     (let syntmp-doloop-694 ((syntmp-i-695 0))
                                       (if (syntmp-fx=-90
                                             syntmp-i-695
                                             syntmp-n-692)
                                         syntmp-v-693
                                         (begin
                                           (vector-set!
                                             syntmp-v-693
                                             syntmp-i-695
                                             (syntmp-rebuild-macro-output-686
                                               (vector-ref
                                                 syntmp-x-687
                                                 syntmp-i-695)
                                               syntmp-m-688))
                                           (syntmp-doloop-694
                                             (syntmp-fx+-88
                                               syntmp-i-695
                                               1))))))))
                                ((symbol? syntmp-x-687)
                                 (syntax-error
                                   syntmp-x-687
                                   "encountered raw symbol in macro output"))
                                (else syntmp-x-687)))))
               (syntmp-rebuild-macro-output-686
                 (syntmp-p-681
                   (syntmp-wrap-146
                     syntmp-e-682
                     (syntmp-anti-mark-133 syntmp-w-684)))
                 (string #\m)))))
         (syntmp-chi-application-156
           (lambda (syntmp-x-696
                    syntmp-e-697
                    syntmp-r-698
                    syntmp-w-699
                    syntmp-s-700)
             ((lambda (syntmp-tmp-701)
                ((lambda (syntmp-tmp-702)
                   (if syntmp-tmp-702
                     (apply (lambda (syntmp-e0-703 syntmp-e1-704)
                              (cons syntmp-x-696
                                    (map (lambda (syntmp-e-705)
                                           (syntmp-chi-154
                                             syntmp-e-705
                                             syntmp-r-698
                                             syntmp-w-699))
                                         syntmp-e1-704)))
                            syntmp-tmp-702)
                     (syntax-error syntmp-tmp-701)))
                 (syntax-dispatch
                   syntmp-tmp-701
                   '(any . each-any))))
              syntmp-e-697)))
         (syntmp-chi-expr-155
           (lambda (syntmp-type-707
                    syntmp-value-708
                    syntmp-e-709
                    syntmp-r-710
                    syntmp-w-711
                    syntmp-s-712)
             (let ((syntmp-t-713 syntmp-type-707))
               (if (memv syntmp-t-713 (quote (lexical)))
                 syntmp-value-708
                 (if (memv syntmp-t-713 (quote (core external-macro)))
                   (syntmp-value-708
                     syntmp-e-709
                     syntmp-r-710
                     syntmp-w-711
                     syntmp-s-712)
                   (if (memv syntmp-t-713 (quote (lexical-call)))
                     (syntmp-chi-application-156
                       syntmp-value-708
                       syntmp-e-709
                       syntmp-r-710
                       syntmp-w-711
                       syntmp-s-712)
                     (if (memv syntmp-t-713 (quote (global-call)))
                       (syntmp-chi-application-156
                         syntmp-value-708
                         syntmp-e-709
                         syntmp-r-710
                         syntmp-w-711
                         syntmp-s-712)
                       (if (memv syntmp-t-713 (quote (constant)))
                         (syntmp-build-data-98
                           syntmp-s-712
                           (syntmp-strip-165
                             (syntmp-source-wrap-147
                               syntmp-e-709
                               syntmp-w-711
                               syntmp-s-712)
                             '(())))
                         (if (memv syntmp-t-713 (quote (global)))
                           syntmp-value-708
                           (if (memv syntmp-t-713 (quote (call)))
                             (syntmp-chi-application-156
                               (syntmp-chi-154
                                 (car syntmp-e-709)
                                 syntmp-r-710
                                 syntmp-w-711)
                               syntmp-e-709
                               syntmp-r-710
                               syntmp-w-711
                               syntmp-s-712)
                             (if (memv syntmp-t-713 (quote (begin-form)))
                               ((lambda (syntmp-tmp-714)
                                  ((lambda (syntmp-tmp-715)
                                     (if syntmp-tmp-715
                                       (apply (lambda (syntmp-_-716
                                                       syntmp-e1-717
                                                       syntmp-e2-718)
                                                (syntmp-chi-sequence-148
                                                  (cons syntmp-e1-717
                                                        syntmp-e2-718)
                                                  syntmp-r-710
                                                  syntmp-w-711
                                                  syntmp-s-712))
                                              syntmp-tmp-715)
                                       (syntax-error syntmp-tmp-714)))
                                   (syntax-dispatch
                                     syntmp-tmp-714
                                     '(any any . each-any))))
                                syntmp-e-709)
                               (if (memv syntmp-t-713
                                         '(local-syntax-form))
                                 (syntmp-chi-local-syntax-160
                                   syntmp-value-708
                                   syntmp-e-709
                                   syntmp-r-710
                                   syntmp-w-711
                                   syntmp-s-712
                                   syntmp-chi-sequence-148)
                                 (if (memv syntmp-t-713
                                           '(eval-when-form))
                                   ((lambda (syntmp-tmp-720)
                                      ((lambda (syntmp-tmp-721)
                                         (if syntmp-tmp-721
                                           (apply (lambda (syntmp-_-722
                                                           syntmp-x-723
                                                           syntmp-e1-724
                                                           syntmp-e2-725)
                                                    (let ((syntmp-when-list-726
                                                            (syntmp-chi-when-list-151
                                                              syntmp-e-709
                                                              syntmp-x-723
                                                              syntmp-w-711)))
                                                      (if (memq 'eval
                                                                syntmp-when-list-726)
                                                        (syntmp-chi-sequence-148
                                                          (cons syntmp-e1-724
                                                                syntmp-e2-725)
                                                          syntmp-r-710
                                                          syntmp-w-711
                                                          syntmp-s-712)
                                                        (syntmp-chi-void-162))))
                                                  syntmp-tmp-721)
                                           (syntax-error syntmp-tmp-720)))
                                       (syntax-dispatch
                                         syntmp-tmp-720
                                         '(any each-any any . each-any))))
                                    syntmp-e-709)
                                   (if (memv syntmp-t-713
                                             '(define-form define-syntax-form))
                                     (syntax-error
                                       (syntmp-wrap-146
                                         syntmp-value-708
                                         syntmp-w-711)
                                       "invalid context for definition of")
                                     (if (memv syntmp-t-713 (quote (syntax)))
                                       (syntax-error
                                         (syntmp-source-wrap-147
                                           syntmp-e-709
                                           syntmp-w-711
                                           syntmp-s-712)
                                         "reference to pattern variable outside syntax form")
                                       (if (memv syntmp-t-713
                                                 '(displaced-lexical))
                                         (syntax-error
                                           (syntmp-source-wrap-147
                                             syntmp-e-709
                                             syntmp-w-711
                                             syntmp-s-712)
                                           "reference to identifier outside its scope")
                                         (syntax-error
                                           (syntmp-source-wrap-147
                                             syntmp-e-709
                                             syntmp-w-711
                                             syntmp-s-712))))))))))))))))))
         (syntmp-chi-154
           (lambda (syntmp-e-729 syntmp-r-730 syntmp-w-731)
             (call-with-values
               (lambda ()
                 (syntmp-syntax-type-152
                   syntmp-e-729
                   syntmp-r-730
                   syntmp-w-731
                   #f
                   #f))
               (lambda (syntmp-type-732
                        syntmp-value-733
                        syntmp-e-734
                        syntmp-w-735
                        syntmp-s-736)
                 (syntmp-chi-expr-155
                   syntmp-type-732
                   syntmp-value-733
                   syntmp-e-734
                   syntmp-r-730
                   syntmp-w-735
                   syntmp-s-736)))))
         (syntmp-chi-top-153
           (lambda (syntmp-e-737
                    syntmp-r-738
                    syntmp-w-739
                    syntmp-m-740
                    syntmp-esew-741)
             (call-with-values
               (lambda ()
                 (syntmp-syntax-type-152
                   syntmp-e-737
                   syntmp-r-738
                   syntmp-w-739
                   #f
                   #f))
               (lambda (syntmp-type-754
                        syntmp-value-755
                        syntmp-e-756
                        syntmp-w-757
                        syntmp-s-758)
                 (let ((syntmp-t-759 syntmp-type-754))
                   (if (memv syntmp-t-759 (quote (begin-form)))
                     ((lambda (syntmp-tmp-760)
                        ((lambda (syntmp-tmp-761)
                           (if syntmp-tmp-761
                             (apply (lambda (syntmp-_-762)
                                      (syntmp-chi-void-162))
                                    syntmp-tmp-761)
                             ((lambda (syntmp-tmp-763)
                                (if syntmp-tmp-763
                                  (apply (lambda (syntmp-_-764
                                                  syntmp-e1-765
                                                  syntmp-e2-766)
                                           (syntmp-chi-top-sequence-149
                                             (cons syntmp-e1-765 syntmp-e2-766)
                                             syntmp-r-738
                                             syntmp-w-757
                                             syntmp-s-758
                                             syntmp-m-740
                                             syntmp-esew-741))
                                         syntmp-tmp-763)
                                  (syntax-error syntmp-tmp-760)))
                              (syntax-dispatch
                                syntmp-tmp-760
                                '(any any . each-any)))))
                         (syntax-dispatch syntmp-tmp-760 (quote (any)))))
                      syntmp-e-756)
                     (if (memv syntmp-t-759 (quote (local-syntax-form)))
                       (syntmp-chi-local-syntax-160
                         syntmp-value-755
                         syntmp-e-756
                         syntmp-r-738
                         syntmp-w-757
                         syntmp-s-758
                         (lambda (syntmp-body-768
                                  syntmp-r-769
                                  syntmp-w-770
                                  syntmp-s-771)
                           (syntmp-chi-top-sequence-149
                             syntmp-body-768
                             syntmp-r-769
                             syntmp-w-770
                             syntmp-s-771
                             syntmp-m-740
                             syntmp-esew-741)))
                       (if (memv syntmp-t-759 (quote (eval-when-form)))
                         ((lambda (syntmp-tmp-772)
                            ((lambda (syntmp-tmp-773)
                               (if syntmp-tmp-773
                                 (apply (lambda (syntmp-_-774
                                                 syntmp-x-775
                                                 syntmp-e1-776
                                                 syntmp-e2-777)
                                          (let ((syntmp-when-list-778
                                                  (syntmp-chi-when-list-151
                                                    syntmp-e-756
                                                    syntmp-x-775
                                                    syntmp-w-757))
                                                (syntmp-body-779
                                                  (cons syntmp-e1-776
                                                        syntmp-e2-777)))
                                            (cond ((eq? syntmp-m-740 (quote e))
                                                   (if (memq 'eval
                                                             syntmp-when-list-778)
                                                     (syntmp-chi-top-sequence-149
                                                       syntmp-body-779
                                                       syntmp-r-738
                                                       syntmp-w-757
                                                       syntmp-s-758
                                                       'e
                                                       '(eval))
                                                     (syntmp-chi-void-162)))
                                                  ((memq 'load
                                                         syntmp-when-list-778)
                                                   (if (or (memq 'compile
                                                                 syntmp-when-list-778)
                                                           (and (eq? syntmp-m-740
                                                                     'c&e)
                                                                (memq 'eval
                                                                      syntmp-when-list-778)))
                                                     (syntmp-chi-top-sequence-149
                                                       syntmp-body-779
                                                       syntmp-r-738
                                                       syntmp-w-757
                                                       syntmp-s-758
                                                       'c&e
                                                       '(compile load))
                                                     (if (memq syntmp-m-740
                                                               '(c c&e))
                                                       (syntmp-chi-top-sequence-149
                                                         syntmp-body-779
                                                         syntmp-r-738
                                                         syntmp-w-757
                                                         syntmp-s-758
                                                         'c
                                                         '(load))
                                                       (syntmp-chi-void-162))))
                                                  ((or (memq 'compile
                                                             syntmp-when-list-778)
                                                       (and (eq? syntmp-m-740
                                                                 'c&e)
                                                            (memq 'eval
                                                                  syntmp-when-list-778)))
                                                   (syntmp-top-level-eval-hook-93
                                                     (syntmp-chi-top-sequence-149
                                                       syntmp-body-779
                                                       syntmp-r-738
                                                       syntmp-w-757
                                                       syntmp-s-758
                                                       'e
                                                       '(eval)))
                                                   (syntmp-chi-void-162))
                                                  (else
                                                   (syntmp-chi-void-162)))))
                                        syntmp-tmp-773)
                                 (syntax-error syntmp-tmp-772)))
                             (syntax-dispatch
                               syntmp-tmp-772
                               '(any each-any any . each-any))))
                          syntmp-e-756)
                         (if (memv syntmp-t-759 (quote (define-syntax-form)))
                           (let ((syntmp-n-782
                                   (syntmp-id-var-name-140
                                     syntmp-value-755
                                     syntmp-w-757))
                                 (syntmp-r-783
                                   (syntmp-macros-only-env-114 syntmp-r-738)))
                             (let ((syntmp-t-784 syntmp-m-740))
                               (if (memv syntmp-t-784 (quote (c)))
                                 (if (memq (quote compile) syntmp-esew-741)
                                   (let ((syntmp-e-785
                                           (syntmp-chi-install-global-150
                                             syntmp-n-782
                                             (syntmp-chi-154
                                               syntmp-e-756
                                               syntmp-r-783
                                               syntmp-w-757))))
                                     (begin
                                       (syntmp-top-level-eval-hook-93
                                         syntmp-e-785)
                                       (if (memq (quote load) syntmp-esew-741)
                                         syntmp-e-785
                                         (syntmp-chi-void-162))))
                                   (if (memq (quote load) syntmp-esew-741)
                                     (syntmp-chi-install-global-150
                                       syntmp-n-782
                                       (syntmp-chi-154
                                         syntmp-e-756
                                         syntmp-r-783
                                         syntmp-w-757))
                                     (syntmp-chi-void-162)))
                                 (if (memv syntmp-t-784 (quote (c&e)))
                                   (let ((syntmp-e-786
                                           (syntmp-chi-install-global-150
                                             syntmp-n-782
                                             (syntmp-chi-154
                                               syntmp-e-756
                                               syntmp-r-783
                                               syntmp-w-757))))
                                     (begin
                                       (syntmp-top-level-eval-hook-93
                                         syntmp-e-786)
                                       syntmp-e-786))
                                   (begin
                                     (if (memq (quote eval) syntmp-esew-741)
                                       (syntmp-top-level-eval-hook-93
                                         (syntmp-chi-install-global-150
                                           syntmp-n-782
                                           (syntmp-chi-154
                                             syntmp-e-756
                                             syntmp-r-783
                                             syntmp-w-757))))
                                     (syntmp-chi-void-162))))))
                           (if (memv syntmp-t-759 (quote (define-form)))
                             (let ((syntmp-n-787
                                     (syntmp-id-var-name-140
                                       syntmp-value-755
                                       syntmp-w-757)))
                               (let ((syntmp-type-788
                                       (syntmp-binding-type-110
                                         (syntmp-lookup-115
                                           syntmp-n-787
                                           syntmp-r-738))))
                                 (let ((syntmp-t-789 syntmp-type-788))
                                   (if (memv syntmp-t-789 (quote (global)))
                                     (let ((syntmp-x-790
                                             (list 'define
                                                   syntmp-n-787
                                                   (syntmp-chi-154
                                                     syntmp-e-756
                                                     syntmp-r-738
                                                     syntmp-w-757))))
                                       (begin
                                         (if (eq? syntmp-m-740 (quote c&e))
                                           (syntmp-top-level-eval-hook-93
                                             syntmp-x-790))
                                         syntmp-x-790))
                                     (if (memv syntmp-t-789
                                               '(displaced-lexical))
                                       (syntax-error
                                         (syntmp-wrap-146
                                           syntmp-value-755
                                           syntmp-w-757)
                                         "identifier out of context")
                                       (if (eq? syntmp-type-788
                                                'external-macro)
                                         (let ((syntmp-x-791
                                                 (list 'define
                                                       syntmp-n-787
                                                       (syntmp-chi-154
                                                         syntmp-e-756
                                                         syntmp-r-738
                                                         syntmp-w-757))))
                                           (begin
                                             (if (eq? syntmp-m-740 (quote c&e))
                                               (syntmp-top-level-eval-hook-93
                                                 syntmp-x-791))
                                             syntmp-x-791))
                                         (syntax-error
                                           (syntmp-wrap-146
                                             syntmp-value-755
                                             syntmp-w-757)
                                           "cannot define keyword at top level")))))))
                             (let ((syntmp-x-792
                                     (syntmp-chi-expr-155
                                       syntmp-type-754
                                       syntmp-value-755
                                       syntmp-e-756
                                       syntmp-r-738
                                       syntmp-w-757
                                       syntmp-s-758)))
                               (begin
                                 (if (eq? syntmp-m-740 (quote c&e))
                                   (syntmp-top-level-eval-hook-93
                                     syntmp-x-792))
                                 syntmp-x-792))))))))))))
         (syntmp-syntax-type-152
           (lambda (syntmp-e-793
                    syntmp-r-794
                    syntmp-w-795
                    syntmp-s-796
                    syntmp-rib-797)
             (cond ((symbol? syntmp-e-793)
                    (let ((syntmp-n-798
                            (syntmp-id-var-name-140
                              syntmp-e-793
                              syntmp-w-795)))
                      (let ((syntmp-b-799
                              (syntmp-lookup-115 syntmp-n-798 syntmp-r-794)))
                        (let ((syntmp-type-800
                                (syntmp-binding-type-110 syntmp-b-799)))
                          (let ((syntmp-t-801 syntmp-type-800))
                            (if (memv syntmp-t-801 (quote (lexical)))
                              (values
                                syntmp-type-800
                                (syntmp-binding-value-111 syntmp-b-799)
                                syntmp-e-793
                                syntmp-w-795
                                syntmp-s-796)
                              (if (memv syntmp-t-801 (quote (global)))
                                (values
                                  syntmp-type-800
                                  syntmp-n-798
                                  syntmp-e-793
                                  syntmp-w-795
                                  syntmp-s-796)
                                (if (memv syntmp-t-801 (quote (macro)))
                                  (syntmp-syntax-type-152
                                    (syntmp-chi-macro-157
                                      (syntmp-binding-value-111 syntmp-b-799)
                                      syntmp-e-793
                                      syntmp-r-794
                                      syntmp-w-795
                                      syntmp-rib-797)
                                    syntmp-r-794
                                    '(())
                                    syntmp-s-796
                                    syntmp-rib-797)
                                  (values
                                    syntmp-type-800
                                    (syntmp-binding-value-111 syntmp-b-799)
                                    syntmp-e-793
                                    syntmp-w-795
                                    syntmp-s-796)))))))))
                   ((pair? syntmp-e-793)
                    (let ((syntmp-first-802 (car syntmp-e-793)))
                      (if (syntmp-id?-118 syntmp-first-802)
                        (let ((syntmp-n-803
                                (syntmp-id-var-name-140
                                  syntmp-first-802
                                  syntmp-w-795)))
                          (let ((syntmp-b-804
                                  (syntmp-lookup-115
                                    syntmp-n-803
                                    syntmp-r-794)))
                            (let ((syntmp-type-805
                                    (syntmp-binding-type-110 syntmp-b-804)))
                              (let ((syntmp-t-806 syntmp-type-805))
                                (if (memv syntmp-t-806 (quote (lexical)))
                                  (values
                                    'lexical-call
                                    (syntmp-binding-value-111 syntmp-b-804)
                                    syntmp-e-793
                                    syntmp-w-795
                                    syntmp-s-796)
                                  (if (memv syntmp-t-806 (quote (global)))
                                    (values
                                      'global-call
                                      syntmp-n-803
                                      syntmp-e-793
                                      syntmp-w-795
                                      syntmp-s-796)
                                    (if (memv syntmp-t-806 (quote (macro)))
                                      (syntmp-syntax-type-152
                                        (syntmp-chi-macro-157
                                          (syntmp-binding-value-111
                                            syntmp-b-804)
                                          syntmp-e-793
                                          syntmp-r-794
                                          syntmp-w-795
                                          syntmp-rib-797)
                                        syntmp-r-794
                                        '(())
                                        syntmp-s-796
                                        syntmp-rib-797)
                                      (if (memv syntmp-t-806
                                                '(core external-macro))
                                        (values
                                          syntmp-type-805
                                          (syntmp-binding-value-111
                                            syntmp-b-804)
                                          syntmp-e-793
                                          syntmp-w-795
                                          syntmp-s-796)
                                        (if (memv syntmp-t-806
                                                  '(local-syntax))
                                          (values
                                            'local-syntax-form
                                            (syntmp-binding-value-111
                                              syntmp-b-804)
                                            syntmp-e-793
                                            syntmp-w-795
                                            syntmp-s-796)
                                          (if (memv syntmp-t-806
                                                    '(begin))
                                            (values
                                              'begin-form
                                              #f
                                              syntmp-e-793
                                              syntmp-w-795
                                              syntmp-s-796)
                                            (if (memv syntmp-t-806
                                                      '(eval-when))
                                              (values
                                                'eval-when-form
                                                #f
                                                syntmp-e-793
                                                syntmp-w-795
                                                syntmp-s-796)
                                              (if (memv syntmp-t-806
                                                        '(define))
                                                ((lambda (syntmp-tmp-807)
                                                   ((lambda (syntmp-tmp-808)
                                                      (if (if syntmp-tmp-808
                                                            (apply (lambda (syntmp-_-809
                                                                            syntmp-name-810
                                                                            syntmp-val-811)
                                                                     (syntmp-id?-118
                                                                       syntmp-name-810))
                                                                   syntmp-tmp-808)
                                                            #f)
                                                        (apply (lambda (syntmp-_-812
                                                                        syntmp-name-813
                                                                        syntmp-val-814)
                                                                 (values
                                                                   'define-form
                                                                   syntmp-name-813
                                                                   syntmp-val-814
                                                                   syntmp-w-795
                                                                   syntmp-s-796))
                                                               syntmp-tmp-808)
                                                        ((lambda (syntmp-tmp-815)
                                                           (if (if syntmp-tmp-815
                                                                 (apply (lambda (syntmp-_-816
                                                                                 syntmp-name-817
                                                                                 syntmp-args-818
                                                                                 syntmp-e1-819
                                                                                 syntmp-e2-820)
                                                                          (and (syntmp-id?-118
                                                                                 syntmp-name-817)
                                                                               (syntmp-valid-bound-ids?-143
                                                                                 (syntmp-lambda-var-list-167
                                                                                   syntmp-args-818))))
                                                                        syntmp-tmp-815)
                                                                 #f)
                                                             (apply (lambda (syntmp-_-821
                                                                             syntmp-name-822
                                                                             syntmp-args-823
                                                                             syntmp-e1-824
                                                                             syntmp-e2-825)
                                                                      (values
                                                                        'define-form
                                                                        (syntmp-wrap-146
                                                                          syntmp-name-822
                                                                          syntmp-w-795)
                                                                        (cons '#(syntax-object
                                                                                 lambda
                                                                                 ((top)
                                                                                  #(ribcage
                                                                                    #(_
                                                                                      name
                                                                                      args
                                                                                      e1
                                                                                      e2)
                                                                                    #((top)
                                                                                      (top)
                                                                                      (top)
                                                                                      (top)
                                                                                      (top))
                                                                                    #("i"
                                                                                      "i"
                                                                                      "i"
                                                                                      "i"
                                                                                      "i"))
                                                                                  #(ribcage
                                                                                    ()
                                                                                    ()
                                                                                    ())
                                                                                  #(ribcage
                                                                                    #(t)
                                                                                    #(("m"
                                                                                       top))
                                                                                    #("i"))
                                                                                  #(ribcage
                                                                                    ()
                                                                                    ()
                                                                                    ())
                                                                                  #(ribcage
                                                                                    ()
                                                                                    ()
                                                                                    ())
                                                                                  #(ribcage
                                                                                    ()
                                                                                    ()
                                                                                    ())
                                                                                  #(ribcage
                                                                                    #(type)
                                                                                    #((top))
                                                                                    #("i"))
                                                                                  #(ribcage
                                                                                    ()
                                                                                    ()
                                                                                    ())
                                                                                  #(ribcage
                                                                                    #(b)
                                                                                    #((top))
                                                                                    #("i"))
                                                                                  #(ribcage
                                                                                    ()
                                                                                    ()
                                                                                    ())
                                                                                  #(ribcage
                                                                                    #(n)
                                                                                    #((top))
                                                                                    #("i"))
                                                                                  #(ribcage
                                                                                    ()
                                                                                    ()
                                                                                    ())
                                                                                  #(ribcage
                                                                                    #(first)
                                                                                    #((top))
                                                                                    #("i"))
                                                                                  #(ribcage
                                                                                    ()
                                                                                    ()
                                                                                    ())
                                                                                  #(ribcage
                                                                                    #(e
                                                                                      r
                                                                                      w
                                                                                      s
                                                                                      rib)
                                                                                    #((top)
                                                                                      (top)
                                                                                      (top)
                                                                                      (top)
                                                                                      (top))
                                                                                    #("i"
                                                                                      "i"
                                                                                      "i"
                                                                                      "i"
                                                                                      "i"))
                                                                                  #(ribcage
                                                                                    (lambda-var-list
                                                                                      gen-var
                                                                                      strip
                                                                                      strip-annotation
                                                                                      ellipsis?
                                                                                      chi-void
                                                                                      eval-local-transformer
                                                                                      chi-local-syntax
                                                                                      chi-lambda-clause
                                                                                      chi-body
                                                                                      chi-macro
                                                                                      chi-application
                                                                                      chi-expr
                                                                                      chi
                                                                                      chi-top
                                                                                      syntax-type
                                                                                      chi-when-list
                                                                                      chi-install-global
                                                                                      chi-top-sequence
                                                                                      chi-sequence
                                                                                      source-wrap
                                                                                      wrap
                                                                                      bound-id-member?
                                                                                      distinct-bound-ids?
                                                                                      valid-bound-ids?
                                                                                      bound-id=?
                                                                                      free-id=?
                                                                                      id-var-name
                                                                                      same-marks?
                                                                                      join-marks
                                                                                      join-wraps
                                                                                      smart-append
                                                                                      make-binding-wrap
                                                                                      extend-ribcage!
                                                                                      make-empty-ribcage
                                                                                      new-mark
                                                                                      anti-mark
                                                                                      the-anti-mark
                                                                                      top-marked?
                                                                                      top-wrap
                                                                                      empty-wrap
                                                                                      set-ribcage-labels!
                                                                                      set-ribcage-marks!
                                                                                      set-ribcage-symnames!
                                                                                      ribcage-labels
                                                                                      ribcage-marks
                                                                                      ribcage-symnames
                                                                                      ribcage?
                                                                                      make-ribcage
                                                                                      gen-labels
                                                                                      gen-label
                                                                                      make-rename
                                                                                      rename-marks
                                                                                      rename-new
                                                                                      rename-old
                                                                                      subst-rename?
                                                                                      wrap-subst
                                                                                      wrap-marks
                                                                                      make-wrap
                                                                                      id-sym-name&marks
                                                                                      id-sym-name
                                                                                      id?
                                                                                      nonsymbol-id?
                                                                                      global-extend
                                                                                      lookup
                                                                                      macros-only-env
                                                                                      extend-var-env
                                                                                      extend-env
                                                                                      null-env
                                                                                      binding-value
                                                                                      binding-type
                                                                                      make-binding
                                                                                      arg-check
                                                                                      source-annotation
                                                                                      no-source
                                                                                      unannotate
                                                                                      set-syntax-object-wrap!
                                                                                      set-syntax-object-expression!
                                                                                      syntax-object-wrap
                                                                                      syntax-object-expression
                                                                                      syntax-object?
                                                                                      make-syntax-object
                                                                                      build-lexical-var
                                                                                      build-letrec
                                                                                      build-named-let
                                                                                      build-let
                                                                                      build-sequence
                                                                                      build-data
                                                                                      build-primref
                                                                                      build-lambda
                                                                                      build-global-definition
                                                                                      build-global-assignment
                                                                                      build-global-reference
                                                                                      build-lexical-assignment
                                                                                      build-lexical-reference
                                                                                      build-conditional
                                                                                      build-application
                                                                                      get-global-definition-hook
                                                                                      put-global-definition-hook
                                                                                      gensym-hook
                                                                                      error-hook
                                                                                      local-eval-hook
                                                                                      top-level-eval-hook
                                                                                      annotation?
                                                                                      fx<
                                                                                      fx=
                                                                                      fx-
                                                                                      fx+
                                                                                      noexpand)
                                                                                    ((top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top))
                                                                                    ("i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"))
                                                                                  #(ribcage
                                                                                    (define-structure)
                                                                                    ((top))
                                                                                    ("i"))))
                                                                              (syntmp-wrap-146
                                                                                (cons syntmp-args-823
                                                                                      (cons syntmp-e1-824
                                                                                            syntmp-e2-825))
                                                                                syntmp-w-795))
                                                                        '(())
                                                                        syntmp-s-796))
                                                                    syntmp-tmp-815)
                                                             ((lambda (syntmp-tmp-827)
                                                                (if (if syntmp-tmp-827
                                                                      (apply (lambda (syntmp-_-828
                                                                                      syntmp-name-829)
                                                                               (syntmp-id?-118
                                                                                 syntmp-name-829))
                                                                             syntmp-tmp-827)
                                                                      #f)
                                                                  (apply (lambda (syntmp-_-830
                                                                                  syntmp-name-831)
                                                                           (values
                                                                             'define-form
                                                                             (syntmp-wrap-146
                                                                               syntmp-name-831
                                                                               syntmp-w-795)
                                                                             '(#(syntax-object
                                                                                 void
                                                                                 ((top)
                                                                                  #(ribcage
                                                                                    #(_
                                                                                      name)
                                                                                    #((top)
                                                                                      (top))
                                                                                    #("i"
                                                                                      "i"))
                                                                                  #(ribcage
                                                                                    ()
                                                                                    ()
                                                                                    ())
                                                                                  #(ribcage
                                                                                    #(t)
                                                                                    #(("m"
                                                                                       top))
                                                                                    #("i"))
                                                                                  #(ribcage
                                                                                    ()
                                                                                    ()
                                                                                    ())
                                                                                  #(ribcage
                                                                                    ()
                                                                                    ()
                                                                                    ())
                                                                                  #(ribcage
                                                                                    ()
                                                                                    ()
                                                                                    ())
                                                                                  #(ribcage
                                                                                    #(type)
                                                                                    #((top))
                                                                                    #("i"))
                                                                                  #(ribcage
                                                                                    ()
                                                                                    ()
                                                                                    ())
                                                                                  #(ribcage
                                                                                    #(b)
                                                                                    #((top))
                                                                                    #("i"))
                                                                                  #(ribcage
                                                                                    ()
                                                                                    ()
                                                                                    ())
                                                                                  #(ribcage
                                                                                    #(n)
                                                                                    #((top))
                                                                                    #("i"))
                                                                                  #(ribcage
                                                                                    ()
                                                                                    ()
                                                                                    ())
                                                                                  #(ribcage
                                                                                    #(first)
                                                                                    #((top))
                                                                                    #("i"))
                                                                                  #(ribcage
                                                                                    ()
                                                                                    ()
                                                                                    ())
                                                                                  #(ribcage
                                                                                    #(e
                                                                                      r
                                                                                      w
                                                                                      s
                                                                                      rib)
                                                                                    #((top)
                                                                                      (top)
                                                                                      (top)
                                                                                      (top)
                                                                                      (top))
                                                                                    #("i"
                                                                                      "i"
                                                                                      "i"
                                                                                      "i"
                                                                                      "i"))
                                                                                  #(ribcage
                                                                                    (lambda-var-list
                                                                                      gen-var
                                                                                      strip
                                                                                      strip-annotation
                                                                                      ellipsis?
                                                                                      chi-void
                                                                                      eval-local-transformer
                                                                                      chi-local-syntax
                                                                                      chi-lambda-clause
                                                                                      chi-body
                                                                                      chi-macro
                                                                                      chi-application
                                                                                      chi-expr
                                                                                      chi
                                                                                      chi-top
                                                                                      syntax-type
                                                                                      chi-when-list
                                                                                      chi-install-global
                                                                                      chi-top-sequence
                                                                                      chi-sequence
                                                                                      source-wrap
                                                                                      wrap
                                                                                      bound-id-member?
                                                                                      distinct-bound-ids?
                                                                                      valid-bound-ids?
                                                                                      bound-id=?
                                                                                      free-id=?
                                                                                      id-var-name
                                                                                      same-marks?
                                                                                      join-marks
                                                                                      join-wraps
                                                                                      smart-append
                                                                                      make-binding-wrap
                                                                                      extend-ribcage!
                                                                                      make-empty-ribcage
                                                                                      new-mark
                                                                                      anti-mark
                                                                                      the-anti-mark
                                                                                      top-marked?
                                                                                      top-wrap
                                                                                      empty-wrap
                                                                                      set-ribcage-labels!
                                                                                      set-ribcage-marks!
                                                                                      set-ribcage-symnames!
                                                                                      ribcage-labels
                                                                                      ribcage-marks
                                                                                      ribcage-symnames
                                                                                      ribcage?
                                                                                      make-ribcage
                                                                                      gen-labels
                                                                                      gen-label
                                                                                      make-rename
                                                                                      rename-marks
                                                                                      rename-new
                                                                                      rename-old
                                                                                      subst-rename?
                                                                                      wrap-subst
                                                                                      wrap-marks
                                                                                      make-wrap
                                                                                      id-sym-name&marks
                                                                                      id-sym-name
                                                                                      id?
                                                                                      nonsymbol-id?
                                                                                      global-extend
                                                                                      lookup
                                                                                      macros-only-env
                                                                                      extend-var-env
                                                                                      extend-env
                                                                                      null-env
                                                                                      binding-value
                                                                                      binding-type
                                                                                      make-binding
                                                                                      arg-check
                                                                                      source-annotation
                                                                                      no-source
                                                                                      unannotate
                                                                                      set-syntax-object-wrap!
                                                                                      set-syntax-object-expression!
                                                                                      syntax-object-wrap
                                                                                      syntax-object-expression
                                                                                      syntax-object?
                                                                                      make-syntax-object
                                                                                      build-lexical-var
                                                                                      build-letrec
                                                                                      build-named-let
                                                                                      build-let
                                                                                      build-sequence
                                                                                      build-data
                                                                                      build-primref
                                                                                      build-lambda
                                                                                      build-global-definition
                                                                                      build-global-assignment
                                                                                      build-global-reference
                                                                                      build-lexical-assignment
                                                                                      build-lexical-reference
                                                                                      build-conditional
                                                                                      build-application
                                                                                      get-global-definition-hook
                                                                                      put-global-definition-hook
                                                                                      gensym-hook
                                                                                      error-hook
                                                                                      local-eval-hook
                                                                                      top-level-eval-hook
                                                                                      annotation?
                                                                                      fx<
                                                                                      fx=
                                                                                      fx-
                                                                                      fx+
                                                                                      noexpand)
                                                                                    ((top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top)
                                                                                     (top))
                                                                                    ("i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"
                                                                                     "i"))
                                                                                  #(ribcage
                                                                                    (define-structure)
                                                                                    ((top))
                                                                                    ("i")))))
                                                                             '(())
                                                                             syntmp-s-796))
                                                                         syntmp-tmp-827)
                                                                  (syntax-error
                                                                    syntmp-tmp-807)))
                                                              (syntax-dispatch
                                                                syntmp-tmp-807
                                                                '(any any)))))
                                                         (syntax-dispatch
                                                           syntmp-tmp-807
                                                           '(any (any . any)
                                                                 any
                                                                 .
                                                                 each-any)))))
                                                    (syntax-dispatch
                                                      syntmp-tmp-807
                                                      '(any any any))))
                                                 syntmp-e-793)
                                                (if (memv syntmp-t-806
                                                          '(define-syntax))
                                                  ((lambda (syntmp-tmp-832)
                                                     ((lambda (syntmp-tmp-833)
                                                        (if (if syntmp-tmp-833
                                                              (apply (lambda (syntmp-_-834
                                                                              syntmp-name-835
                                                                              syntmp-val-836)
                                                                       (syntmp-id?-118
                                                                         syntmp-name-835))
                                                                     syntmp-tmp-833)
                                                              #f)
                                                          (apply (lambda (syntmp-_-837
                                                                          syntmp-name-838
                                                                          syntmp-val-839)
                                                                   (values
                                                                     'define-syntax-form
                                                                     syntmp-name-838
                                                                     syntmp-val-839
                                                                     syntmp-w-795
                                                                     syntmp-s-796))
                                                                 syntmp-tmp-833)
                                                          (syntax-error
                                                            syntmp-tmp-832)))
                                                      (syntax-dispatch
                                                        syntmp-tmp-832
                                                        '(any any any))))
                                                   syntmp-e-793)
                                                  (if (memv syntmp-t-806
                                                            '(ellipsis))
                                                    (values
                                                      'ellipsis
                                                      (syntmp-make-syntax-object-103
                                                        (syntmp-syntax-object-expression-105
                                                          value)
                                                        (syntmp-anti-mark-133
                                                          (syntmp-syntax-object-wrap-106
                                                            value))))
                                                    (values
                                                      'call
                                                      #f
                                                      syntmp-e-793
                                                      syntmp-w-795
                                                      syntmp-s-796)))))))))))))))
                        (values
                          'call
                          #f
                          syntmp-e-793
                          syntmp-w-795
                          syntmp-s-796))))
                   ((syntmp-syntax-object?-104 syntmp-e-793)
                    (syntmp-syntax-type-152
                      (syntmp-syntax-object-expression-105
                        syntmp-e-793)
                      syntmp-r-794
                      (syntmp-join-wraps-137
                        syntmp-w-795
                        (syntmp-syntax-object-wrap-106 syntmp-e-793))
                      #f
                      syntmp-rib-797))
                   ((syntmp-annotation?-92 syntmp-e-793)
                    (syntmp-syntax-type-152
                      (annotation-expression syntmp-e-793)
                      syntmp-r-794
                      syntmp-w-795
                      (annotation-source syntmp-e-793)
                      syntmp-rib-797))
                   ((self-evaluating? syntmp-e-793)
                    (values
                      'constant
                      #f
                      syntmp-e-793
                      syntmp-w-795
                      syntmp-s-796))
                   (else
                    (values
                      'other
                      #f
                      syntmp-e-793
                      syntmp-w-795
                      syntmp-s-796)))))
         (syntmp-chi-when-list-151
           (lambda (syntmp-e-840 syntmp-when-list-841 syntmp-w-842)
             (let syntmp-f-843 ((syntmp-when-list-844 syntmp-when-list-841)
                                (syntmp-situations-845 (quote ())))
               (if (null? syntmp-when-list-844)
                 syntmp-situations-845
                 (syntmp-f-843
                   (cdr syntmp-when-list-844)
                   (cons (let ((syntmp-x-846 (car syntmp-when-list-844)))
                           (cond ((syntmp-free-id=?-141
                                    syntmp-x-846
                                    '#(syntax-object
                                       compile
                                       ((top)
                                        #(ribcage () () ())
                                        #(ribcage #(x) #((top)) #("i"))
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(f when-list situations)
                                          #((top) (top) (top))
                                          #("i" "i" "i"))
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(e when-list w)
                                          #((top) (top) (top))
                                          #("i" "i" "i"))
                                        #(ribcage
                                          (lambda-var-list
                                            gen-var
                                            strip
                                            strip-annotation
                                            ellipsis?
                                            chi-void
                                            eval-local-transformer
                                            chi-local-syntax
                                            chi-lambda-clause
                                            chi-body
                                            chi-macro
                                            chi-application
                                            chi-expr
                                            chi
                                            chi-top
                                            syntax-type
                                            chi-when-list
                                            chi-install-global
                                            chi-top-sequence
                                            chi-sequence
                                            source-wrap
                                            wrap
                                            bound-id-member?
                                            distinct-bound-ids?
                                            valid-bound-ids?
                                            bound-id=?
                                            free-id=?
                                            id-var-name
                                            same-marks?
                                            join-marks
                                            join-wraps
                                            smart-append
                                            make-binding-wrap
                                            extend-ribcage!
                                            make-empty-ribcage
                                            new-mark
                                            anti-mark
                                            the-anti-mark
                                            top-marked?
                                            top-wrap
                                            empty-wrap
                                            set-ribcage-labels!
                                            set-ribcage-marks!
                                            set-ribcage-symnames!
                                            ribcage-labels
                                            ribcage-marks
                                            ribcage-symnames
                                            ribcage?
                                            make-ribcage
                                            gen-labels
                                            gen-label
                                            make-rename
                                            rename-marks
                                            rename-new
                                            rename-old
                                            subst-rename?
                                            wrap-subst
                                            wrap-marks
                                            make-wrap
                                            id-sym-name&marks
                                            id-sym-name
                                            id?
                                            nonsymbol-id?
                                            global-extend
                                            lookup
                                            macros-only-env
                                            extend-var-env
                                            extend-env
                                            null-env
                                            binding-value
                                            binding-type
                                            make-binding
                                            arg-check
                                            source-annotation
                                            no-source
                                            unannotate
                                            set-syntax-object-wrap!
                                            set-syntax-object-expression!
                                            syntax-object-wrap
                                            syntax-object-expression
                                            syntax-object?
                                            make-syntax-object
                                            build-lexical-var
                                            build-letrec
                                            build-named-let
                                            build-let
                                            build-sequence
                                            build-data
                                            build-primref
                                            build-lambda
                                            build-global-definition
                                            build-global-assignment
                                            build-global-reference
                                            build-lexical-assignment
                                            build-lexical-reference
                                            build-conditional
                                            build-application
                                            get-global-definition-hook
                                            put-global-definition-hook
                                            gensym-hook
                                            error-hook
                                            local-eval-hook
                                            top-level-eval-hook
                                            annotation?
                                            fx<
                                            fx=
                                            fx-
                                            fx+
                                            noexpand)
                                          ((top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top))
                                          ("i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"))
                                        #(ribcage
                                          (define-structure)
                                          ((top))
                                          ("i")))))
                                  'compile)
                                 ((syntmp-free-id=?-141
                                    syntmp-x-846
                                    '#(syntax-object
                                       load
                                       ((top)
                                        #(ribcage () () ())
                                        #(ribcage #(x) #((top)) #("i"))
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(f when-list situations)
                                          #((top) (top) (top))
                                          #("i" "i" "i"))
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(e when-list w)
                                          #((top) (top) (top))
                                          #("i" "i" "i"))
                                        #(ribcage
                                          (lambda-var-list
                                            gen-var
                                            strip
                                            strip-annotation
                                            ellipsis?
                                            chi-void
                                            eval-local-transformer
                                            chi-local-syntax
                                            chi-lambda-clause
                                            chi-body
                                            chi-macro
                                            chi-application
                                            chi-expr
                                            chi
                                            chi-top
                                            syntax-type
                                            chi-when-list
                                            chi-install-global
                                            chi-top-sequence
                                            chi-sequence
                                            source-wrap
                                            wrap
                                            bound-id-member?
                                            distinct-bound-ids?
                                            valid-bound-ids?
                                            bound-id=?
                                            free-id=?
                                            id-var-name
                                            same-marks?
                                            join-marks
                                            join-wraps
                                            smart-append
                                            make-binding-wrap
                                            extend-ribcage!
                                            make-empty-ribcage
                                            new-mark
                                            anti-mark
                                            the-anti-mark
                                            top-marked?
                                            top-wrap
                                            empty-wrap
                                            set-ribcage-labels!
                                            set-ribcage-marks!
                                            set-ribcage-symnames!
                                            ribcage-labels
                                            ribcage-marks
                                            ribcage-symnames
                                            ribcage?
                                            make-ribcage
                                            gen-labels
                                            gen-label
                                            make-rename
                                            rename-marks
                                            rename-new
                                            rename-old
                                            subst-rename?
                                            wrap-subst
                                            wrap-marks
                                            make-wrap
                                            id-sym-name&marks
                                            id-sym-name
                                            id?
                                            nonsymbol-id?
                                            global-extend
                                            lookup
                                            macros-only-env
                                            extend-var-env
                                            extend-env
                                            null-env
                                            binding-value
                                            binding-type
                                            make-binding
                                            arg-check
                                            source-annotation
                                            no-source
                                            unannotate
                                            set-syntax-object-wrap!
                                            set-syntax-object-expression!
                                            syntax-object-wrap
                                            syntax-object-expression
                                            syntax-object?
                                            make-syntax-object
                                            build-lexical-var
                                            build-letrec
                                            build-named-let
                                            build-let
                                            build-sequence
                                            build-data
                                            build-primref
                                            build-lambda
                                            build-global-definition
                                            build-global-assignment
                                            build-global-reference
                                            build-lexical-assignment
                                            build-lexical-reference
                                            build-conditional
                                            build-application
                                            get-global-definition-hook
                                            put-global-definition-hook
                                            gensym-hook
                                            error-hook
                                            local-eval-hook
                                            top-level-eval-hook
                                            annotation?
                                            fx<
                                            fx=
                                            fx-
                                            fx+
                                            noexpand)
                                          ((top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top))
                                          ("i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"))
                                        #(ribcage
                                          (define-structure)
                                          ((top))
                                          ("i")))))
                                  'load)
                                 ((syntmp-free-id=?-141
                                    syntmp-x-846
                                    '#(syntax-object
                                       eval
                                       ((top)
                                        #(ribcage () () ())
                                        #(ribcage #(x) #((top)) #("i"))
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(f when-list situations)
                                          #((top) (top) (top))
                                          #("i" "i" "i"))
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(e when-list w)
                                          #((top) (top) (top))
                                          #("i" "i" "i"))
                                        #(ribcage
                                          (lambda-var-list
                                            gen-var
                                            strip
                                            strip-annotation
                                            ellipsis?
                                            chi-void
                                            eval-local-transformer
                                            chi-local-syntax
                                            chi-lambda-clause
                                            chi-body
                                            chi-macro
                                            chi-application
                                            chi-expr
                                            chi
                                            chi-top
                                            syntax-type
                                            chi-when-list
                                            chi-install-global
                                            chi-top-sequence
                                            chi-sequence
                                            source-wrap
                                            wrap
                                            bound-id-member?
                                            distinct-bound-ids?
                                            valid-bound-ids?
                                            bound-id=?
                                            free-id=?
                                            id-var-name
                                            same-marks?
                                            join-marks
                                            join-wraps
                                            smart-append
                                            make-binding-wrap
                                            extend-ribcage!
                                            make-empty-ribcage
                                            new-mark
                                            anti-mark
                                            the-anti-mark
                                            top-marked?
                                            top-wrap
                                            empty-wrap
                                            set-ribcage-labels!
                                            set-ribcage-marks!
                                            set-ribcage-symnames!
                                            ribcage-labels
                                            ribcage-marks
                                            ribcage-symnames
                                            ribcage?
                                            make-ribcage
                                            gen-labels
                                            gen-label
                                            make-rename
                                            rename-marks
                                            rename-new
                                            rename-old
                                            subst-rename?
                                            wrap-subst
                                            wrap-marks
                                            make-wrap
                                            id-sym-name&marks
                                            id-sym-name
                                            id?
                                            nonsymbol-id?
                                            global-extend
                                            lookup
                                            macros-only-env
                                            extend-var-env
                                            extend-env
                                            null-env
                                            binding-value
                                            binding-type
                                            make-binding
                                            arg-check
                                            source-annotation
                                            no-source
                                            unannotate
                                            set-syntax-object-wrap!
                                            set-syntax-object-expression!
                                            syntax-object-wrap
                                            syntax-object-expression
                                            syntax-object?
                                            make-syntax-object
                                            build-lexical-var
                                            build-letrec
                                            build-named-let
                                            build-let
                                            build-sequence
                                            build-data
                                            build-primref
                                            build-lambda
                                            build-global-definition
                                            build-global-assignment
                                            build-global-reference
                                            build-lexical-assignment
                                            build-lexical-reference
                                            build-conditional
                                            build-application
                                            get-global-definition-hook
                                            put-global-definition-hook
                                            gensym-hook
                                            error-hook
                                            local-eval-hook
                                            top-level-eval-hook
                                            annotation?
                                            fx<
                                            fx=
                                            fx-
                                            fx+
                                            noexpand)
                                          ((top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top))
                                          ("i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"
                                           "i"))
                                        #(ribcage
                                          (define-structure)
                                          ((top))
                                          ("i")))))
                                  'eval)
                                 (else
                                  (syntax-error
                                    (syntmp-wrap-146 syntmp-x-846 syntmp-w-842)
                                    "invalid eval-when situation"))))
                         syntmp-situations-845))))))
         (syntmp-chi-install-global-150
           (lambda (syntmp-name-847 syntmp-e-848)
             (list 'install-global-transformer
                   (syntmp-build-data-98 #f syntmp-name-847)
                   syntmp-e-848)))
         (syntmp-chi-top-sequence-149
           (lambda (syntmp-body-849
                    syntmp-r-850
                    syntmp-w-851
                    syntmp-s-852
                    syntmp-m-853
                    syntmp-esew-854)
             (syntmp-build-sequence-99
               syntmp-s-852
               (let syntmp-dobody-855 ((syntmp-body-856 syntmp-body-849)
                                       (syntmp-r-857 syntmp-r-850)
                                       (syntmp-w-858 syntmp-w-851)
                                       (syntmp-m-859 syntmp-m-853)
                                       (syntmp-esew-860 syntmp-esew-854))
                 (if (null? syntmp-body-856)
                   '()
                   (let ((syntmp-first-861
                           (syntmp-chi-top-153
                             (car syntmp-body-856)
                             syntmp-r-857
                             syntmp-w-858
                             syntmp-m-859
                             syntmp-esew-860)))
                     (cons syntmp-first-861
                           (syntmp-dobody-855
                             (cdr syntmp-body-856)
                             syntmp-r-857
                             syntmp-w-858
                             syntmp-m-859
                             syntmp-esew-860))))))))
         (syntmp-chi-sequence-148
           (lambda (syntmp-body-862
                    syntmp-r-863
                    syntmp-w-864
                    syntmp-s-865)
             (syntmp-build-sequence-99
               syntmp-s-865
               (let syntmp-dobody-866 ((syntmp-body-867 syntmp-body-862)
                                       (syntmp-r-868 syntmp-r-863)
                                       (syntmp-w-869 syntmp-w-864))
                 (if (null? syntmp-body-867)
                   '()
                   (let ((syntmp-first-870
                           (syntmp-chi-154
                             (car syntmp-body-867)
                             syntmp-r-868
                             syntmp-w-869)))
                     (cons syntmp-first-870
                           (syntmp-dobody-866
                             (cdr syntmp-body-867)
                             syntmp-r-868
                             syntmp-w-869))))))))
         (syntmp-source-wrap-147
           (lambda (syntmp-x-871 syntmp-w-872 syntmp-s-873)
             (syntmp-wrap-146
               (if syntmp-s-873
                 (make-annotation syntmp-x-871 syntmp-s-873 #f)
                 syntmp-x-871)
               syntmp-w-872)))
         (syntmp-wrap-146
           (lambda (syntmp-x-874 syntmp-w-875)
             (cond ((and (null? (syntmp-wrap-marks-121 syntmp-w-875))
                         (null? (syntmp-wrap-subst-122 syntmp-w-875)))
                    syntmp-x-874)
                   ((syntmp-syntax-object?-104 syntmp-x-874)
                    (syntmp-make-syntax-object-103
                      (syntmp-syntax-object-expression-105
                        syntmp-x-874)
                      (syntmp-join-wraps-137
                        syntmp-w-875
                        (syntmp-syntax-object-wrap-106 syntmp-x-874))))
                   ((null? syntmp-x-874) syntmp-x-874)
                   (else
                    (syntmp-make-syntax-object-103
                      syntmp-x-874
                      syntmp-w-875)))))
         (syntmp-bound-id-member?-145
           (lambda (syntmp-x-876 syntmp-list-877)
             (and (not (null? syntmp-list-877))
                  (or (syntmp-bound-id=?-142
                        syntmp-x-876
                        (car syntmp-list-877))
                      (syntmp-bound-id-member?-145
                        syntmp-x-876
                        (cdr syntmp-list-877))))))
         (syntmp-distinct-bound-ids?-144
           (lambda (syntmp-ids-878)
             (let syntmp-distinct?-879 ((syntmp-ids-880 syntmp-ids-878))
               (or (null? syntmp-ids-880)
                   (and (not (syntmp-bound-id-member?-145
                               (car syntmp-ids-880)
                               (cdr syntmp-ids-880)))
                        (syntmp-distinct?-879 (cdr syntmp-ids-880)))))))
         (syntmp-valid-bound-ids?-143
           (lambda (syntmp-ids-881)
             (and (let syntmp-all-ids?-882 ((syntmp-ids-883 syntmp-ids-881))
                    (or (null? syntmp-ids-883)
                        (and (syntmp-id?-118 (car syntmp-ids-883))
                             (syntmp-all-ids?-882 (cdr syntmp-ids-883)))))
                  (syntmp-distinct-bound-ids?-144 syntmp-ids-881))))
         (syntmp-bound-id=?-142
           (lambda (syntmp-i-884 syntmp-j-885)
             (if (and (syntmp-syntax-object?-104 syntmp-i-884)
                      (syntmp-syntax-object?-104 syntmp-j-885))
               (and (eq? (let ((syntmp-e-886
                                 (syntmp-syntax-object-expression-105
                                   syntmp-i-884)))
                           (if (syntmp-annotation?-92 syntmp-e-886)
                             (annotation-expression syntmp-e-886)
                             syntmp-e-886))
                         (let ((syntmp-e-887
                                 (syntmp-syntax-object-expression-105
                                   syntmp-j-885)))
                           (if (syntmp-annotation?-92 syntmp-e-887)
                             (annotation-expression syntmp-e-887)
                             syntmp-e-887)))
                    (syntmp-same-marks?-139
                      (syntmp-wrap-marks-121
                        (syntmp-syntax-object-wrap-106 syntmp-i-884))
                      (syntmp-wrap-marks-121
                        (syntmp-syntax-object-wrap-106 syntmp-j-885))))
               (eq? (let ((syntmp-e-888 syntmp-i-884))
                      (if (syntmp-annotation?-92 syntmp-e-888)
                        (annotation-expression syntmp-e-888)
                        syntmp-e-888))
                    (let ((syntmp-e-889 syntmp-j-885))
                      (if (syntmp-annotation?-92 syntmp-e-889)
                        (annotation-expression syntmp-e-889)
                        syntmp-e-889))))))
         (syntmp-free-id=?-141
           (lambda (syntmp-i-890 syntmp-j-891)
             (and (eq? (let ((syntmp-x-892 syntmp-i-890))
                         (let ((syntmp-e-893
                                 (if (syntmp-syntax-object?-104 syntmp-x-892)
                                   (syntmp-syntax-object-expression-105
                                     syntmp-x-892)
                                   syntmp-x-892)))
                           (if (syntmp-annotation?-92 syntmp-e-893)
                             (annotation-expression syntmp-e-893)
                             syntmp-e-893)))
                       (let ((syntmp-x-894 syntmp-j-891))
                         (let ((syntmp-e-895
                                 (if (syntmp-syntax-object?-104 syntmp-x-894)
                                   (syntmp-syntax-object-expression-105
                                     syntmp-x-894)
                                   syntmp-x-894)))
                           (if (syntmp-annotation?-92 syntmp-e-895)
                             (annotation-expression syntmp-e-895)
                             syntmp-e-895))))
                  (eq? (syntmp-id-var-name-140
                         syntmp-i-890
                         '(()))
                       (syntmp-id-var-name-140
                         syntmp-j-891
                         '(()))))))
         (syntmp-id-var-name-140
           (lambda (syntmp-id-896 syntmp-w-897)
             (letrec ((syntmp-search-vector-rib-900
                        (lambda (syntmp-sym-911
                                 syntmp-subst-912
                                 syntmp-marks-913
                                 syntmp-symnames-914
                                 syntmp-ribcage-915)
                          (let ((syntmp-n-916
                                  (vector-length syntmp-symnames-914)))
                            (let syntmp-f-917 ((syntmp-i-918 0))
                              (cond ((syntmp-fx=-90 syntmp-i-918 syntmp-n-916)
                                     (syntmp-search-898
                                       syntmp-sym-911
                                       (cdr syntmp-subst-912)
                                       syntmp-marks-913))
                                    ((and (eq? (vector-ref
                                                 syntmp-symnames-914
                                                 syntmp-i-918)
                                               syntmp-sym-911)
                                          (syntmp-same-marks?-139
                                            syntmp-marks-913
                                            (vector-ref
                                              (syntmp-ribcage-marks-128
                                                syntmp-ribcage-915)
                                              syntmp-i-918)))
                                     (values
                                       (vector-ref
                                         (syntmp-ribcage-labels-129
                                           syntmp-ribcage-915)
                                         syntmp-i-918)
                                       syntmp-marks-913))
                                    (else
                                     (syntmp-f-917
                                       (syntmp-fx+-88 syntmp-i-918 1))))))))
                      (syntmp-search-list-rib-899
                        (lambda (syntmp-sym-919
                                 syntmp-subst-920
                                 syntmp-marks-921
                                 syntmp-symnames-922
                                 syntmp-ribcage-923)
                          (let syntmp-f-924 ((syntmp-symnames-925
                                               syntmp-symnames-922)
                                             (syntmp-i-926 0))
                            (cond ((null? syntmp-symnames-925)
                                   (syntmp-search-898
                                     syntmp-sym-919
                                     (cdr syntmp-subst-920)
                                     syntmp-marks-921))
                                  ((and (eq? (car syntmp-symnames-925)
                                             syntmp-sym-919)
                                        (syntmp-same-marks?-139
                                          syntmp-marks-921
                                          (list-ref
                                            (syntmp-ribcage-marks-128
                                              syntmp-ribcage-923)
                                            syntmp-i-926)))
                                   (values
                                     (list-ref
                                       (syntmp-ribcage-labels-129
                                         syntmp-ribcage-923)
                                       syntmp-i-926)
                                     syntmp-marks-921))
                                  (else
                                   (syntmp-f-924
                                     (cdr syntmp-symnames-925)
                                     (syntmp-fx+-88 syntmp-i-926 1)))))))
                      (syntmp-search-898
                        (lambda (syntmp-sym-927
                                 syntmp-subst-928
                                 syntmp-marks-929)
                          (if (null? syntmp-subst-928)
                            (values #f syntmp-marks-929)
                            (let ((syntmp-fst-930 (car syntmp-subst-928)))
                              (if (eq? syntmp-fst-930 (quote shift))
                                (syntmp-search-898
                                  syntmp-sym-927
                                  (cdr syntmp-subst-928)
                                  (cdr syntmp-marks-929))
                                (let ((syntmp-symnames-931
                                        (syntmp-ribcage-symnames-127
                                          syntmp-fst-930)))
                                  (if (vector? syntmp-symnames-931)
                                    (syntmp-search-vector-rib-900
                                      syntmp-sym-927
                                      syntmp-subst-928
                                      syntmp-marks-929
                                      syntmp-symnames-931
                                      syntmp-fst-930)
                                    (syntmp-search-list-rib-899
                                      syntmp-sym-927
                                      syntmp-subst-928
                                      syntmp-marks-929
                                      syntmp-symnames-931
                                      syntmp-fst-930)))))))))
               (cond ((symbol? syntmp-id-896)
                      (or (call-with-values
                            (lambda ()
                              (syntmp-search-898
                                syntmp-id-896
                                (syntmp-wrap-subst-122 syntmp-w-897)
                                (syntmp-wrap-marks-121 syntmp-w-897)))
                            (lambda (syntmp-x-933 . syntmp-ignore-932)
                              syntmp-x-933))
                          syntmp-id-896))
                     ((syntmp-syntax-object?-104 syntmp-id-896)
                      (let ((syntmp-id-934
                              (let ((syntmp-e-936
                                      (syntmp-syntax-object-expression-105
                                        syntmp-id-896)))
                                (if (syntmp-annotation?-92 syntmp-e-936)
                                  (annotation-expression syntmp-e-936)
                                  syntmp-e-936)))
                            (syntmp-w1-935
                              (syntmp-syntax-object-wrap-106 syntmp-id-896)))
                        (let ((syntmp-marks-937
                                (syntmp-join-marks-138
                                  (syntmp-wrap-marks-121 syntmp-w-897)
                                  (syntmp-wrap-marks-121 syntmp-w1-935))))
                          (call-with-values
                            (lambda ()
                              (syntmp-search-898
                                syntmp-id-934
                                (syntmp-wrap-subst-122 syntmp-w-897)
                                syntmp-marks-937))
                            (lambda (syntmp-new-id-938 syntmp-marks-939)
                              (or syntmp-new-id-938
                                  (call-with-values
                                    (lambda ()
                                      (syntmp-search-898
                                        syntmp-id-934
                                        (syntmp-wrap-subst-122 syntmp-w1-935)
                                        syntmp-marks-939))
                                    (lambda (syntmp-x-941 . syntmp-ignore-940)
                                      syntmp-x-941))
                                  syntmp-id-934))))))
                     ((syntmp-annotation?-92 syntmp-id-896)
                      (let ((syntmp-id-942
                              (let ((syntmp-e-943 syntmp-id-896))
                                (if (syntmp-annotation?-92 syntmp-e-943)
                                  (annotation-expression syntmp-e-943)
                                  syntmp-e-943))))
                        (or (call-with-values
                              (lambda ()
                                (syntmp-search-898
                                  syntmp-id-942
                                  (syntmp-wrap-subst-122 syntmp-w-897)
                                  (syntmp-wrap-marks-121 syntmp-w-897)))
                              (lambda (syntmp-x-945 . syntmp-ignore-944)
                                syntmp-x-945))
                            syntmp-id-942)))
                     (else
                      (syntmp-error-hook-95
                        'id-var-name
                        "invalid id"
                        syntmp-id-896))))))
         (syntmp-same-marks?-139
           (lambda (syntmp-x-946 syntmp-y-947)
             (or (eq? syntmp-x-946 syntmp-y-947)
                 (and (not (null? syntmp-x-946))
                      (not (null? syntmp-y-947))
                      (eq? (car syntmp-x-946) (car syntmp-y-947))
                      (syntmp-same-marks?-139
                        (cdr syntmp-x-946)
                        (cdr syntmp-y-947))))))
         (syntmp-join-marks-138
           (lambda (syntmp-m1-948 syntmp-m2-949)
             (syntmp-smart-append-136
               syntmp-m1-948
               syntmp-m2-949)))
         (syntmp-join-wraps-137
           (lambda (syntmp-w1-950 syntmp-w2-951)
             (let ((syntmp-m1-952
                     (syntmp-wrap-marks-121 syntmp-w1-950))
                   (syntmp-s1-953
                     (syntmp-wrap-subst-122 syntmp-w1-950)))
               (if (null? syntmp-m1-952)
                 (if (null? syntmp-s1-953)
                   syntmp-w2-951
                   (syntmp-make-wrap-120
                     (syntmp-wrap-marks-121 syntmp-w2-951)
                     (syntmp-smart-append-136
                       syntmp-s1-953
                       (syntmp-wrap-subst-122 syntmp-w2-951))))
                 (syntmp-make-wrap-120
                   (syntmp-smart-append-136
                     syntmp-m1-952
                     (syntmp-wrap-marks-121 syntmp-w2-951))
                   (syntmp-smart-append-136
                     syntmp-s1-953
                     (syntmp-wrap-subst-122 syntmp-w2-951)))))))
         (syntmp-smart-append-136
           (lambda (syntmp-m1-954 syntmp-m2-955)
             (if (null? syntmp-m2-955)
               syntmp-m1-954
               (append syntmp-m1-954 syntmp-m2-955))))
         (syntmp-make-binding-wrap-135
           (lambda (syntmp-ids-956 syntmp-labels-957 syntmp-w-958)
             (if (null? syntmp-ids-956)
               syntmp-w-958
               (syntmp-make-wrap-120
                 (syntmp-wrap-marks-121 syntmp-w-958)
                 (cons (let ((syntmp-labelvec-959
                               (list->vector syntmp-labels-957)))
                         (let ((syntmp-n-960
                                 (vector-length syntmp-labelvec-959)))
                           (let ((syntmp-symnamevec-961
                                   (make-vector syntmp-n-960))
                                 (syntmp-marksvec-962
                                   (make-vector syntmp-n-960)))
                             (begin
                               (let syntmp-f-963 ((syntmp-ids-964
                                                    syntmp-ids-956)
                                                  (syntmp-i-965 0))
                                 (if (not (null? syntmp-ids-964))
                                   (call-with-values
                                     (lambda ()
                                       (syntmp-id-sym-name&marks-119
                                         (car syntmp-ids-964)
                                         syntmp-w-958))
                                     (lambda (syntmp-symname-966
                                              syntmp-marks-967)
                                       (begin
                                         (vector-set!
                                           syntmp-symnamevec-961
                                           syntmp-i-965
                                           syntmp-symname-966)
                                         (vector-set!
                                           syntmp-marksvec-962
                                           syntmp-i-965
                                           syntmp-marks-967)
                                         (syntmp-f-963
                                           (cdr syntmp-ids-964)
                                           (syntmp-fx+-88 syntmp-i-965 1)))))))
                               (syntmp-make-ribcage-125
                                 syntmp-symnamevec-961
                                 syntmp-marksvec-962
                                 syntmp-labelvec-959)))))
                       (syntmp-wrap-subst-122 syntmp-w-958))))))
         (syntmp-extend-ribcage!-134
           (lambda (syntmp-ribcage-968
                    syntmp-id-969
                    syntmp-label-970)
             (begin
               (syntmp-set-ribcage-symnames!-130
                 syntmp-ribcage-968
                 (cons (let ((syntmp-e-971
                               (syntmp-syntax-object-expression-105
                                 syntmp-id-969)))
                         (if (syntmp-annotation?-92 syntmp-e-971)
                           (annotation-expression syntmp-e-971)
                           syntmp-e-971))
                       (syntmp-ribcage-symnames-127 syntmp-ribcage-968)))
               (syntmp-set-ribcage-marks!-131
                 syntmp-ribcage-968
                 (cons (syntmp-wrap-marks-121
                         (syntmp-syntax-object-wrap-106 syntmp-id-969))
                       (syntmp-ribcage-marks-128 syntmp-ribcage-968)))
               (syntmp-set-ribcage-labels!-132
                 syntmp-ribcage-968
                 (cons syntmp-label-970
                       (syntmp-ribcage-labels-129 syntmp-ribcage-968))))))
         (syntmp-anti-mark-133
           (lambda (syntmp-w-972)
             (syntmp-make-wrap-120
               (cons #f (syntmp-wrap-marks-121 syntmp-w-972))
               (cons 'shift
                     (syntmp-wrap-subst-122 syntmp-w-972)))))
         (syntmp-set-ribcage-labels!-132
           (lambda (syntmp-x-973 syntmp-update-974)
             (vector-set! syntmp-x-973 3 syntmp-update-974)))
         (syntmp-set-ribcage-marks!-131
           (lambda (syntmp-x-975 syntmp-update-976)
             (vector-set! syntmp-x-975 2 syntmp-update-976)))
         (syntmp-set-ribcage-symnames!-130
           (lambda (syntmp-x-977 syntmp-update-978)
             (vector-set! syntmp-x-977 1 syntmp-update-978)))
         (syntmp-ribcage-labels-129
           (lambda (syntmp-x-979)
             (vector-ref syntmp-x-979 3)))
         (syntmp-ribcage-marks-128
           (lambda (syntmp-x-980)
             (vector-ref syntmp-x-980 2)))
         (syntmp-ribcage-symnames-127
           (lambda (syntmp-x-981)
             (vector-ref syntmp-x-981 1)))
         (syntmp-ribcage?-126
           (lambda (syntmp-x-982)
             (and (vector? syntmp-x-982)
                  (= (vector-length syntmp-x-982) 4)
                  (eq? (vector-ref syntmp-x-982 0) (quote ribcage)))))
         (syntmp-make-ribcage-125
           (lambda (syntmp-symnames-983
                    syntmp-marks-984
                    syntmp-labels-985)
             (vector
               'ribcage
               syntmp-symnames-983
               syntmp-marks-984
               syntmp-labels-985)))
         (syntmp-gen-labels-124
           (lambda (syntmp-ls-986)
             (if (null? syntmp-ls-986)
               '()
               (cons (syntmp-gen-label-123)
                     (syntmp-gen-labels-124 (cdr syntmp-ls-986))))))
         (syntmp-gen-label-123 (lambda () (string #\i)))
         (syntmp-wrap-subst-122 cdr)
         (syntmp-wrap-marks-121 car)
         (syntmp-make-wrap-120 cons)
         (syntmp-id-sym-name&marks-119
           (lambda (syntmp-x-987 syntmp-w-988)
             (if (syntmp-syntax-object?-104 syntmp-x-987)
               (values
                 (let ((syntmp-e-989
                         (syntmp-syntax-object-expression-105
                           syntmp-x-987)))
                   (if (syntmp-annotation?-92 syntmp-e-989)
                     (annotation-expression syntmp-e-989)
                     syntmp-e-989))
                 (syntmp-join-marks-138
                   (syntmp-wrap-marks-121 syntmp-w-988)
                   (syntmp-wrap-marks-121
                     (syntmp-syntax-object-wrap-106 syntmp-x-987))))
               (values
                 (let ((syntmp-e-990 syntmp-x-987))
                   (if (syntmp-annotation?-92 syntmp-e-990)
                     (annotation-expression syntmp-e-990)
                     syntmp-e-990))
                 (syntmp-wrap-marks-121 syntmp-w-988)))))
         (syntmp-id?-118
           (lambda (syntmp-x-991)
             (cond ((symbol? syntmp-x-991) #t)
                   ((syntmp-syntax-object?-104 syntmp-x-991)
                    (symbol?
                      (let ((syntmp-e-992
                              (syntmp-syntax-object-expression-105
                                syntmp-x-991)))
                        (if (syntmp-annotation?-92 syntmp-e-992)
                          (annotation-expression syntmp-e-992)
                          syntmp-e-992))))
                   ((syntmp-annotation?-92 syntmp-x-991)
                    (symbol? (annotation-expression syntmp-x-991)))
                   (else #f))))
         (syntmp-nonsymbol-id?-117
           (lambda (syntmp-x-993)
             (and (syntmp-syntax-object?-104 syntmp-x-993)
                  (symbol?
                    (let ((syntmp-e-994
                            (syntmp-syntax-object-expression-105
                              syntmp-x-993)))
                      (if (syntmp-annotation?-92 syntmp-e-994)
                        (annotation-expression syntmp-e-994)
                        syntmp-e-994))))))
         (syntmp-global-extend-116
           (lambda (syntmp-type-995 syntmp-sym-996 syntmp-val-997)
             (syntmp-put-global-definition-hook-96
               syntmp-sym-996
               (cons syntmp-type-995 syntmp-val-997))))
         (syntmp-lookup-115
           (lambda (syntmp-x-998 syntmp-r-999)
             (cond ((assq syntmp-x-998 syntmp-r-999) => cdr)
                   ((symbol? syntmp-x-998)
                    (or (syntmp-get-global-definition-hook-97
                          syntmp-x-998)
                        '(global)))
                   (else (quote (displaced-lexical))))))
         (syntmp-macros-only-env-114
           (lambda (syntmp-r-1000)
             (if (null? syntmp-r-1000)
               '()
               (let ((syntmp-a-1001 (car syntmp-r-1000)))
                 (if (memq (cadr syntmp-a-1001)
                           '(macro ellipsis))
                   (cons syntmp-a-1001
                         (syntmp-macros-only-env-114 (cdr syntmp-r-1000)))
                   (syntmp-macros-only-env-114 (cdr syntmp-r-1000)))))))
         (syntmp-extend-var-env-113
           (lambda (syntmp-labels-1002
                    syntmp-vars-1003
                    syntmp-r-1004)
             (if (null? syntmp-labels-1002)
               syntmp-r-1004
               (syntmp-extend-var-env-113
                 (cdr syntmp-labels-1002)
                 (cdr syntmp-vars-1003)
                 (cons (cons (car syntmp-labels-1002)
                             (cons (quote lexical) (car syntmp-vars-1003)))
                       syntmp-r-1004)))))
         (syntmp-extend-env-112
           (lambda (syntmp-labels-1005
                    syntmp-bindings-1006
                    syntmp-r-1007)
             (if (null? syntmp-labels-1005)
               syntmp-r-1007
               (syntmp-extend-env-112
                 (cdr syntmp-labels-1005)
                 (cdr syntmp-bindings-1006)
                 (cons (cons (car syntmp-labels-1005)
                             (car syntmp-bindings-1006))
                       syntmp-r-1007)))))
         (syntmp-binding-value-111 cdr)
         (syntmp-binding-type-110 car)
         (syntmp-source-annotation-109
           (lambda (syntmp-x-1008)
             (cond ((syntmp-annotation?-92 syntmp-x-1008)
                    (annotation-source syntmp-x-1008))
                   ((syntmp-syntax-object?-104 syntmp-x-1008)
                    (syntmp-source-annotation-109
                      (syntmp-syntax-object-expression-105
                        syntmp-x-1008)))
                   (else #f))))
         (syntmp-set-syntax-object-wrap!-108
           (lambda (syntmp-x-1009 syntmp-update-1010)
             (vector-set! syntmp-x-1009 2 syntmp-update-1010)))
         (syntmp-set-syntax-object-expression!-107
           (lambda (syntmp-x-1011 syntmp-update-1012)
             (vector-set! syntmp-x-1011 1 syntmp-update-1012)))
         (syntmp-syntax-object-wrap-106
           (lambda (syntmp-x-1013)
             (vector-ref syntmp-x-1013 2)))
         (syntmp-syntax-object-expression-105
           (lambda (syntmp-x-1014)
             (vector-ref syntmp-x-1014 1)))
         (syntmp-syntax-object?-104
           (lambda (syntmp-x-1015)
             (and (vector? syntmp-x-1015)
                  (= (vector-length syntmp-x-1015) 3)
                  (eq? (vector-ref syntmp-x-1015 0)
                       'syntax-object))))
         (syntmp-make-syntax-object-103
           (lambda (syntmp-expression-1016 syntmp-wrap-1017)
             (vector
               'syntax-object
               syntmp-expression-1016
               syntmp-wrap-1017)))
         (syntmp-build-letrec-102
           (lambda (syntmp-src-1018
                    syntmp-vars-1019
                    syntmp-val-exps-1020
                    syntmp-body-exp-1021)
             (if (null? syntmp-vars-1019)
               syntmp-body-exp-1021
               (list 'letrec
                     (map list syntmp-vars-1019 syntmp-val-exps-1020)
                     syntmp-body-exp-1021))))
         (syntmp-build-named-let-101
           (lambda (syntmp-src-1022
                    syntmp-vars-1023
                    syntmp-val-exps-1024
                    syntmp-body-exp-1025)
             (if (null? syntmp-vars-1023)
               syntmp-body-exp-1025
               (list 'let
                     (car syntmp-vars-1023)
                     (map list
                          (cdr syntmp-vars-1023)
                          syntmp-val-exps-1024)
                     syntmp-body-exp-1025))))
         (syntmp-build-let-100
           (lambda (syntmp-src-1026
                    syntmp-vars-1027
                    syntmp-val-exps-1028
                    syntmp-body-exp-1029)
             (if (null? syntmp-vars-1027)
               syntmp-body-exp-1029
               (list 'let
                     (map list syntmp-vars-1027 syntmp-val-exps-1028)
                     syntmp-body-exp-1029))))
         (syntmp-build-sequence-99
           (lambda (syntmp-src-1030 syntmp-exps-1031)
             (if (null? (cdr syntmp-exps-1031))
               (car syntmp-exps-1031)
               (cons (quote begin) syntmp-exps-1031))))
         (syntmp-build-data-98
           (lambda (syntmp-src-1032 syntmp-exp-1033)
             (if (and (self-evaluating? syntmp-exp-1033)
                      (not (vector? syntmp-exp-1033)))
               syntmp-exp-1033
               (list (quote quote) syntmp-exp-1033))))
         (syntmp-get-global-definition-hook-97
           (lambda (syntmp-symbol-1034)
             (getprop
               syntmp-symbol-1034
               '*sc-expander*)))
         (syntmp-put-global-definition-hook-96
           (lambda (syntmp-symbol-1035 syntmp-binding-1036)
             (putprop
               syntmp-symbol-1035
               '*sc-expander*
               syntmp-binding-1036)))
         (syntmp-error-hook-95
           (lambda (syntmp-who-1037
                    syntmp-why-1038
                    syntmp-what-1039)
             (error syntmp-who-1037
                    "~a ~s"
                    syntmp-why-1038
                    syntmp-what-1039)))
         (syntmp-local-eval-hook-94
           (lambda (syntmp-x-1040)
             (eval (list syntmp-noexpand-87 syntmp-x-1040)
                   (interaction-environment))))
         (syntmp-top-level-eval-hook-93
           (lambda (syntmp-x-1041)
             (eval (list syntmp-noexpand-87 syntmp-x-1041)
                   (interaction-environment))))
         (syntmp-annotation?-92
           (lambda (syntmp-x-1042) #f))
         (syntmp-fx<-91 <)
         (syntmp-fx=-90 =)
         (syntmp-fx--89 -)
         (syntmp-fx+-88 +)
         (syntmp-noexpand-87 "noexpand"))
  (begin
    (syntmp-global-extend-116
      'local-syntax
      'letrec-syntax
      #t)
    (syntmp-global-extend-116
      'local-syntax
      'let-syntax
      #f)
    (syntmp-global-extend-116
      'core
      'fluid-let-syntax
      (lambda (syntmp-e-1043
               syntmp-r-1044
               syntmp-w-1045
               syntmp-s-1046)
        ((lambda (syntmp-tmp-1047)
           ((lambda (syntmp-tmp-1048)
              (if (if syntmp-tmp-1048
                    (apply (lambda (syntmp-_-1049
                                    syntmp-var-1050
                                    syntmp-val-1051
                                    syntmp-e1-1052
                                    syntmp-e2-1053)
                             (syntmp-valid-bound-ids?-143 syntmp-var-1050))
                           syntmp-tmp-1048)
                    #f)
                (apply (lambda (syntmp-_-1055
                                syntmp-var-1056
                                syntmp-val-1057
                                syntmp-e1-1058
                                syntmp-e2-1059)
                         (let ((syntmp-names-1060
                                 (map (lambda (syntmp-x-1061)
                                        (syntmp-id-var-name-140
                                          syntmp-x-1061
                                          syntmp-w-1045))
                                      syntmp-var-1056)))
                           (begin
                             (for-each
                               (lambda (syntmp-id-1063 syntmp-n-1064)
                                 (let ((syntmp-t-1065
                                         (syntmp-binding-type-110
                                           (syntmp-lookup-115
                                             syntmp-n-1064
                                             syntmp-r-1044))))
                                   (if (memv syntmp-t-1065
                                             '(displaced-lexical))
                                     (syntax-error
                                       (syntmp-source-wrap-147
                                         syntmp-id-1063
                                         syntmp-w-1045
                                         syntmp-s-1046)
                                       "identifier out of context"))))
                               syntmp-var-1056
                               syntmp-names-1060)
                             (syntmp-chi-body-158
                               (cons syntmp-e1-1058 syntmp-e2-1059)
                               (syntmp-source-wrap-147
                                 syntmp-e-1043
                                 syntmp-w-1045
                                 syntmp-s-1046)
                               (syntmp-extend-env-112
                                 syntmp-names-1060
                                 (let ((syntmp-trans-r-1068
                                         (syntmp-macros-only-env-114
                                           syntmp-r-1044)))
                                   (map (lambda (syntmp-x-1069)
                                          (cons 'macro
                                                (syntmp-eval-local-transformer-161
                                                  (syntmp-chi-154
                                                    syntmp-x-1069
                                                    syntmp-trans-r-1068
                                                    syntmp-w-1045))))
                                        syntmp-val-1057))
                                 syntmp-r-1044)
                               syntmp-w-1045))))
                       syntmp-tmp-1048)
                ((lambda (syntmp-_-1071)
                   (syntax-error
                     (syntmp-source-wrap-147
                       syntmp-e-1043
                       syntmp-w-1045
                       syntmp-s-1046)))
                 syntmp-tmp-1047)))
            (syntax-dispatch
              syntmp-tmp-1047
              '(any #(each (any any)) any . each-any))))
         syntmp-e-1043)))
    (syntmp-global-extend-116
      'core
      'quote
      (lambda (syntmp-e-1072
               syntmp-r-1073
               syntmp-w-1074
               syntmp-s-1075)
        ((lambda (syntmp-tmp-1076)
           ((lambda (syntmp-tmp-1077)
              (if syntmp-tmp-1077
                (apply (lambda (syntmp-_-1078 syntmp-e-1079)
                         (syntmp-build-data-98
                           syntmp-s-1075
                           (syntmp-strip-165 syntmp-e-1079 syntmp-w-1074)))
                       syntmp-tmp-1077)
                ((lambda (syntmp-_-1080)
                   (syntax-error
                     (syntmp-source-wrap-147
                       syntmp-e-1072
                       syntmp-w-1074
                       syntmp-s-1075)))
                 syntmp-tmp-1076)))
            (syntax-dispatch
              syntmp-tmp-1076
              '(any any))))
         syntmp-e-1072)))
    (syntmp-global-extend-116
      'core
      'syntax
      (letrec ((syntmp-regen-1088
                 (lambda (syntmp-x-1089)
                   (let ((syntmp-t-1090 (car syntmp-x-1089)))
                     (if (memv syntmp-t-1090 (quote (ref)))
                       (cadr syntmp-x-1089)
                       (if (memv syntmp-t-1090 (quote (primitive)))
                         (cadr syntmp-x-1089)
                         (if (memv syntmp-t-1090 (quote (quote)))
                           (syntmp-build-data-98 #f (cadr syntmp-x-1089))
                           (if (memv syntmp-t-1090 (quote (lambda)))
                             (list 'lambda
                                   (cadr syntmp-x-1089)
                                   (syntmp-regen-1088 (caddr syntmp-x-1089)))
                             (if (memv syntmp-t-1090 (quote (map)))
                               (let ((syntmp-ls-1091
                                       (map syntmp-regen-1088
                                            (cdr syntmp-x-1089))))
                                 (cons (if (syntmp-fx=-90
                                             (length syntmp-ls-1091)
                                             2)
                                         'map
                                         'map)
                                       syntmp-ls-1091))
                               (cons (car syntmp-x-1089)
                                     (map syntmp-regen-1088
                                          (cdr syntmp-x-1089)))))))))))
               (syntmp-gen-vector-1087
                 (lambda (syntmp-x-1092)
                   (cond ((eq? (car syntmp-x-1092) (quote list))
                          (cons (quote vector) (cdr syntmp-x-1092)))
                         ((eq? (car syntmp-x-1092) (quote quote))
                          (list 'quote
                                (list->vector (cadr syntmp-x-1092))))
                         (else (list (quote list->vector) syntmp-x-1092)))))
               (syntmp-gen-append-1086
                 (lambda (syntmp-x-1093 syntmp-y-1094)
                   (if (equal? syntmp-y-1094 (quote (quote ())))
                     syntmp-x-1093
                     (list (quote append) syntmp-x-1093 syntmp-y-1094))))
               (syntmp-gen-cons-1085
                 (lambda (syntmp-x-1095 syntmp-y-1096)
                   (let ((syntmp-t-1097 (car syntmp-y-1096)))
                     (if (memv syntmp-t-1097 (quote (quote)))
                       (if (eq? (car syntmp-x-1095) (quote quote))
                         (list 'quote
                               (cons (cadr syntmp-x-1095)
                                     (cadr syntmp-y-1096)))
                         (if (eq? (cadr syntmp-y-1096) (quote ()))
                           (list (quote list) syntmp-x-1095)
                           (list (quote cons) syntmp-x-1095 syntmp-y-1096)))
                       (if (memv syntmp-t-1097 (quote (list)))
                         (cons 'list
                               (cons syntmp-x-1095 (cdr syntmp-y-1096)))
                         (list (quote cons) syntmp-x-1095 syntmp-y-1096))))))
               (syntmp-gen-map-1084
                 (lambda (syntmp-e-1098 syntmp-map-env-1099)
                   (let ((syntmp-formals-1100
                           (map cdr syntmp-map-env-1099))
                         (syntmp-actuals-1101
                           (map (lambda (syntmp-x-1102)
                                  (list (quote ref) (car syntmp-x-1102)))
                                syntmp-map-env-1099)))
                     (cond ((eq? (car syntmp-e-1098) (quote ref))
                            (car syntmp-actuals-1101))
                           ((andmap
                              (lambda (syntmp-x-1103)
                                (and (eq? (car syntmp-x-1103) (quote ref))
                                     (memq (cadr syntmp-x-1103)
                                           syntmp-formals-1100)))
                              (cdr syntmp-e-1098))
                            (cons 'map
                                  (cons (list 'primitive
                                              (car syntmp-e-1098))
                                        (map (let ((syntmp-r-1104
                                                     (map cons
                                                          syntmp-formals-1100
                                                          syntmp-actuals-1101)))
                                               (lambda (syntmp-x-1105)
                                                 (cdr (assq (cadr syntmp-x-1105)
                                                            syntmp-r-1104))))
                                             (cdr syntmp-e-1098)))))
                           (else
                            (cons 'map
                                  (cons (list 'lambda
                                              syntmp-formals-1100
                                              syntmp-e-1098)
                                        syntmp-actuals-1101)))))))
               (syntmp-gen-mappend-1083
                 (lambda (syntmp-e-1106 syntmp-map-env-1107)
                   (list 'apply
                         '(primitive append)
                         (syntmp-gen-map-1084
                           syntmp-e-1106
                           syntmp-map-env-1107))))
               (syntmp-gen-ref-1082
                 (lambda (syntmp-src-1108
                          syntmp-var-1109
                          syntmp-level-1110
                          syntmp-maps-1111)
                   (if (syntmp-fx=-90 syntmp-level-1110 0)
                     (values syntmp-var-1109 syntmp-maps-1111)
                     (if (null? syntmp-maps-1111)
                       (syntax-error
                         syntmp-src-1108
                         "missing ellipsis in syntax form")
                       (call-with-values
                         (lambda ()
                           (syntmp-gen-ref-1082
                             syntmp-src-1108
                             syntmp-var-1109
                             (syntmp-fx--89 syntmp-level-1110 1)
                             (cdr syntmp-maps-1111)))
                         (lambda (syntmp-outer-var-1112 syntmp-outer-maps-1113)
                           (let ((syntmp-b-1114
                                   (assq syntmp-outer-var-1112
                                         (car syntmp-maps-1111))))
                             (if syntmp-b-1114
                               (values (cdr syntmp-b-1114) syntmp-maps-1111)
                               (let ((syntmp-inner-var-1115
                                       (syntmp-gen-var-166 (quote tmp))))
                                 (values
                                   syntmp-inner-var-1115
                                   (cons (cons (cons syntmp-outer-var-1112
                                                     syntmp-inner-var-1115)
                                               (car syntmp-maps-1111))
                                         syntmp-outer-maps-1113)))))))))))
               (syntmp-gen-syntax-1081
                 (lambda (syntmp-src-1116
                          syntmp-e-1117
                          syntmp-r-1118
                          syntmp-maps-1119
                          syntmp-ellipsis?-1120)
                   (if (syntmp-id?-118 syntmp-e-1117)
                     (let ((syntmp-label-1121
                             (syntmp-id-var-name-140
                               syntmp-e-1117
                               '(()))))
                       (let ((syntmp-b-1122
                               (syntmp-lookup-115
                                 syntmp-label-1121
                                 syntmp-r-1118)))
                         (if (eq? (syntmp-binding-type-110 syntmp-b-1122)
                                  'syntax)
                           (call-with-values
                             (lambda ()
                               (let ((syntmp-var.lev-1123
                                       (syntmp-binding-value-111
                                         syntmp-b-1122)))
                                 (syntmp-gen-ref-1082
                                   syntmp-src-1116
                                   (car syntmp-var.lev-1123)
                                   (cdr syntmp-var.lev-1123)
                                   syntmp-maps-1119)))
                             (lambda (syntmp-var-1124 syntmp-maps-1125)
                               (values
                                 (list (quote ref) syntmp-var-1124)
                                 syntmp-maps-1125)))
                           (if (syntmp-ellipsis?-1120
                                 syntmp-e-1117
                                 syntmp-r-1118)
                             (syntax-error
                               syntmp-src-1116
                               "misplaced ellipsis in syntax form")
                             (values
                               (list (quote quote) syntmp-e-1117)
                               syntmp-maps-1119)))))
                     ((lambda (syntmp-tmp-1126)
                        ((lambda (syntmp-tmp-1127)
                           (if (if syntmp-tmp-1127
                                 (apply (lambda (syntmp-dots-1128
                                                 syntmp-e-1129)
                                          (syntmp-ellipsis?-1120
                                            syntmp-dots-1128
                                            syntmp-r-1118))
                                        syntmp-tmp-1127)
                                 #f)
                             (apply (lambda (syntmp-dots-1130 syntmp-e-1131)
                                      (syntmp-gen-syntax-1081
                                        syntmp-src-1116
                                        syntmp-e-1131
                                        syntmp-r-1118
                                        syntmp-maps-1119
                                        (lambda (syntmp-e-1132 syntmp-r-1133)
                                          #f)))
                                    syntmp-tmp-1127)
                             ((lambda (syntmp-tmp-1134)
                                (if (if syntmp-tmp-1134
                                      (apply (lambda (syntmp-x-1135
                                                      syntmp-dots-1136
                                                      syntmp-y-1137)
                                               (syntmp-ellipsis?-1120
                                                 syntmp-dots-1136
                                                 syntmp-r-1118))
                                             syntmp-tmp-1134)
                                      #f)
                                  (apply (lambda (syntmp-x-1138
                                                  syntmp-dots-1139
                                                  syntmp-y-1140)
                                           (let syntmp-f-1141 ((syntmp-y-1142
                                                                 syntmp-y-1140)
                                                               (syntmp-k-1143
                                                                 (lambda (syntmp-maps-1144)
                                                                   (call-with-values
                                                                     (lambda ()
                                                                       (syntmp-gen-syntax-1081
                                                                         syntmp-src-1116
                                                                         syntmp-x-1138
                                                                         syntmp-r-1118
                                                                         (cons '()
                                                                               syntmp-maps-1144)
                                                                         syntmp-ellipsis?-1120))
                                                                     (lambda (syntmp-x-1145
                                                                              syntmp-maps-1146)
                                                                       (if (null? (car syntmp-maps-1146))
                                                                         (syntax-error
                                                                           syntmp-src-1116
                                                                           "extra ellipsis in syntax form")
                                                                         (values
                                                                           (syntmp-gen-map-1084
                                                                             syntmp-x-1145
                                                                             (car syntmp-maps-1146))
                                                                           (cdr syntmp-maps-1146))))))))
                                             ((lambda (syntmp-tmp-1147)
                                                ((lambda (syntmp-tmp-1148)
                                                   (if (if syntmp-tmp-1148
                                                         (apply (lambda (syntmp-dots-1149
                                                                         syntmp-y-1150)
                                                                  (syntmp-ellipsis?-1120
                                                                    syntmp-dots-1149
                                                                    syntmp-r-1118))
                                                                syntmp-tmp-1148)
                                                         #f)
                                                     (apply (lambda (syntmp-dots-1151
                                                                     syntmp-y-1152)
                                                              (syntmp-f-1141
                                                                syntmp-y-1152
                                                                (lambda (syntmp-maps-1153)
                                                                  (call-with-values
                                                                    (lambda ()
                                                                      (syntmp-k-1143
                                                                        (cons '()
                                                                              syntmp-maps-1153)))
                                                                    (lambda (syntmp-x-1154
                                                                             syntmp-maps-1155)
                                                                      (if (null? (car syntmp-maps-1155))
                                                                        (syntax-error
                                                                          syntmp-src-1116
                                                                          "extra ellipsis in syntax form")
                                                                        (values
                                                                          (syntmp-gen-mappend-1083
                                                                            syntmp-x-1154
                                                                            (car syntmp-maps-1155))
                                                                          (cdr syntmp-maps-1155))))))))
                                                            syntmp-tmp-1148)
                                                     ((lambda (syntmp-_-1156)
                                                        (call-with-values
                                                          (lambda ()
                                                            (syntmp-gen-syntax-1081
                                                              syntmp-src-1116
                                                              syntmp-y-1142
                                                              syntmp-r-1118
                                                              syntmp-maps-1119
                                                              syntmp-ellipsis?-1120))
                                                          (lambda (syntmp-y-1157
                                                                   syntmp-maps-1158)
                                                            (call-with-values
                                                              (lambda ()
                                                                (syntmp-k-1143
                                                                  syntmp-maps-1158))
                                                              (lambda (syntmp-x-1159
                                                                       syntmp-maps-1160)
                                                                (values
                                                                  (syntmp-gen-append-1086
                                                                    syntmp-x-1159
                                                                    syntmp-y-1157)
                                                                  syntmp-maps-1160))))))
                                                      syntmp-tmp-1147)))
                                                 (syntax-dispatch
                                                   syntmp-tmp-1147
                                                   '(any . any))))
                                              syntmp-y-1142)))
                                         syntmp-tmp-1134)
                                  ((lambda (syntmp-tmp-1161)
                                     (if syntmp-tmp-1161
                                       (apply (lambda (syntmp-x-1162
                                                       syntmp-y-1163)
                                                (call-with-values
                                                  (lambda ()
                                                    (syntmp-gen-syntax-1081
                                                      syntmp-src-1116
                                                      syntmp-x-1162
                                                      syntmp-r-1118
                                                      syntmp-maps-1119
                                                      syntmp-ellipsis?-1120))
                                                  (lambda (syntmp-x-1164
                                                           syntmp-maps-1165)
                                                    (call-with-values
                                                      (lambda ()
                                                        (syntmp-gen-syntax-1081
                                                          syntmp-src-1116
                                                          syntmp-y-1163
                                                          syntmp-r-1118
                                                          syntmp-maps-1165
                                                          syntmp-ellipsis?-1120))
                                                      (lambda (syntmp-y-1166
                                                               syntmp-maps-1167)
                                                        (values
                                                          (syntmp-gen-cons-1085
                                                            syntmp-x-1164
                                                            syntmp-y-1166)
                                                          syntmp-maps-1167))))))
                                              syntmp-tmp-1161)
                                       ((lambda (syntmp-tmp-1168)
                                          (if syntmp-tmp-1168
                                            (apply (lambda (syntmp-e1-1169
                                                            syntmp-e2-1170)
                                                     (call-with-values
                                                       (lambda ()
                                                         (syntmp-gen-syntax-1081
                                                           syntmp-src-1116
                                                           (cons syntmp-e1-1169
                                                                 syntmp-e2-1170)
                                                           syntmp-r-1118
                                                           syntmp-maps-1119
                                                           syntmp-ellipsis?-1120))
                                                       (lambda (syntmp-e-1172
                                                                syntmp-maps-1173)
                                                         (values
                                                           (syntmp-gen-vector-1087
                                                             syntmp-e-1172)
                                                           syntmp-maps-1173))))
                                                   syntmp-tmp-1168)
                                            ((lambda (syntmp-_-1174)
                                               (values
                                                 (list 'quote
                                                       syntmp-e-1117)
                                                 syntmp-maps-1119))
                                             syntmp-tmp-1126)))
                                        (syntax-dispatch
                                          syntmp-tmp-1126
                                          '#(vector (any . each-any))))))
                                   (syntax-dispatch
                                     syntmp-tmp-1126
                                     '(any . any)))))
                              (syntax-dispatch
                                syntmp-tmp-1126
                                '(any any . any)))))
                         (syntax-dispatch
                           syntmp-tmp-1126
                           '(any any))))
                      syntmp-e-1117)))))
        (lambda (syntmp-e-1175
                 syntmp-r-1176
                 syntmp-w-1177
                 syntmp-s-1178)
          (let ((syntmp-e-1179
                  (syntmp-source-wrap-147
                    syntmp-e-1175
                    syntmp-w-1177
                    syntmp-s-1178)))
            ((lambda (syntmp-tmp-1180)
               ((lambda (syntmp-tmp-1181)
                  (if syntmp-tmp-1181
                    (apply (lambda (syntmp-_-1182 syntmp-x-1183)
                             (call-with-values
                               (lambda ()
                                 (syntmp-gen-syntax-1081
                                   syntmp-e-1179
                                   syntmp-x-1183
                                   syntmp-r-1176
                                   '()
                                   syntmp-ellipsis?-163))
                               (lambda (syntmp-e-1184 syntmp-maps-1185)
                                 (syntmp-regen-1088 syntmp-e-1184))))
                           syntmp-tmp-1181)
                    ((lambda (syntmp-_-1186)
                       (syntax-error syntmp-e-1179))
                     syntmp-tmp-1180)))
                (syntax-dispatch
                  syntmp-tmp-1180
                  '(any any))))
             syntmp-e-1179)))))
    (syntmp-global-extend-116
      'core
      'lambda
      (lambda (syntmp-e-1187
               syntmp-r-1188
               syntmp-w-1189
               syntmp-s-1190)
        ((lambda (syntmp-tmp-1191)
           ((lambda (syntmp-tmp-1192)
              (if syntmp-tmp-1192
                (apply (lambda (syntmp-_-1193 syntmp-c-1194)
                         (syntmp-chi-lambda-clause-159
                           (syntmp-source-wrap-147
                             syntmp-e-1187
                             syntmp-w-1189
                             syntmp-s-1190)
                           syntmp-c-1194
                           syntmp-r-1188
                           syntmp-w-1189
                           (lambda (syntmp-vars-1195 syntmp-body-1196)
                             (list 'lambda
                                   syntmp-vars-1195
                                   syntmp-body-1196))))
                       syntmp-tmp-1192)
                (syntax-error syntmp-tmp-1191)))
            (syntax-dispatch
              syntmp-tmp-1191
              '(any . any))))
         syntmp-e-1187)))
    (syntmp-global-extend-116
      'core
      'with-ellipsis
      (lambda (syntmp-e-1197
               syntmp-r-1198
               syntmp-w-1199
               syntmp-s-1200)
        (let ((syntmp-tmp-1201 syntmp-e-1197))
          (let ((syntmp-tmp-1202
                  (syntax-dispatch
                    syntmp-tmp-1201
                    '(_ any any . each-any))))
            (if (and syntmp-tmp-1202
                     (apply (lambda (syntmp-dots-1203
                                     syntmp-e1-1204
                                     syntmp-e2-1205)
                              (syntmp-id?-118 syntmp-dots-1203))
                            syntmp-tmp-1202))
              (apply (lambda (syntmp-dots-1206 syntmp-e1-1207 syntmp-e2-1208)
                       (let ((syntmp-id-1209
                               (if (symbol? syntmp-dots-1206)
                                 '$sc-ellipsis
                                 (syntmp-make-syntax-object-103
                                   '$sc-ellipsis
                                   (syntmp-syntax-object-wrap-106
                                     syntmp-dots-1206)))))
                         (let ((syntmp-ids-1210 (list syntmp-id-1209))
                               (syntmp-labels-1211
                                 (list (syntmp-gen-label-123)))
                               (syntmp-bindings-1212
                                 (list (cons 'ellipsis
                                             (syntmp-source-wrap-147
                                               syntmp-dots-1206
                                               syntmp-w-1199
                                               syntmp-s-1200)))))
                           (let ((syntmp-nw-1213
                                   (syntmp-make-binding-wrap-135
                                     syntmp-ids-1210
                                     syntmp-labels-1211
                                     syntmp-w-1199))
                                 (syntmp-nr-1214
                                   (syntmp-extend-env-112
                                     syntmp-labels-1211
                                     syntmp-bindings-1212
                                     syntmp-r-1198)))
                             (syntmp-chi-body-158
                               (cons syntmp-e1-1207 syntmp-e2-1208)
                               (syntmp-source-wrap-147
                                 syntmp-e-1197
                                 syntmp-nw-1213
                                 syntmp-s-1200)
                               syntmp-nr-1214
                               syntmp-nw-1213)))))
                     syntmp-tmp-1202)
              (syntax-error (quote with-ellipsis) "bad syntax"))))))
    (syntmp-global-extend-116
      'core
      'let
      (letrec ((syntmp-chi-let-1215
                 (lambda (syntmp-e-1216
                          syntmp-r-1217
                          syntmp-w-1218
                          syntmp-s-1219
                          syntmp-constructor-1220
                          syntmp-ids-1221
                          syntmp-vals-1222
                          syntmp-exps-1223)
                   (if (not (syntmp-valid-bound-ids?-143 syntmp-ids-1221))
                     (syntax-error
                       syntmp-e-1216
                       "duplicate bound variable in")
                     (let ((syntmp-labels-1224
                             (syntmp-gen-labels-124 syntmp-ids-1221))
                           (syntmp-new-vars-1225
                             (map syntmp-gen-var-166 syntmp-ids-1221)))
                       (let ((syntmp-nw-1226
                               (syntmp-make-binding-wrap-135
                                 syntmp-ids-1221
                                 syntmp-labels-1224
                                 syntmp-w-1218))
                             (syntmp-nr-1227
                               (syntmp-extend-var-env-113
                                 syntmp-labels-1224
                                 syntmp-new-vars-1225
                                 syntmp-r-1217)))
                         (syntmp-constructor-1220
                           syntmp-s-1219
                           syntmp-new-vars-1225
                           (map (lambda (syntmp-x-1228)
                                  (syntmp-chi-154
                                    syntmp-x-1228
                                    syntmp-r-1217
                                    syntmp-w-1218))
                                syntmp-vals-1222)
                           (syntmp-chi-body-158
                             syntmp-exps-1223
                             (syntmp-source-wrap-147
                               syntmp-e-1216
                               syntmp-nw-1226
                               syntmp-s-1219)
                             syntmp-nr-1227
                             syntmp-nw-1226))))))))
        (lambda (syntmp-e-1229
                 syntmp-r-1230
                 syntmp-w-1231
                 syntmp-s-1232)
          ((lambda (syntmp-tmp-1233)
             ((lambda (syntmp-tmp-1234)
                (if syntmp-tmp-1234
                  (apply (lambda (syntmp-_-1235
                                  syntmp-id-1236
                                  syntmp-val-1237
                                  syntmp-e1-1238
                                  syntmp-e2-1239)
                           (syntmp-chi-let-1215
                             syntmp-e-1229
                             syntmp-r-1230
                             syntmp-w-1231
                             syntmp-s-1232
                             syntmp-build-let-100
                             syntmp-id-1236
                             syntmp-val-1237
                             (cons syntmp-e1-1238 syntmp-e2-1239)))
                         syntmp-tmp-1234)
                  ((lambda (syntmp-tmp-1243)
                     (if (if syntmp-tmp-1243
                           (apply (lambda (syntmp-_-1244
                                           syntmp-f-1245
                                           syntmp-id-1246
                                           syntmp-val-1247
                                           syntmp-e1-1248
                                           syntmp-e2-1249)
                                    (syntmp-id?-118 syntmp-f-1245))
                                  syntmp-tmp-1243)
                           #f)
                       (apply (lambda (syntmp-_-1250
                                       syntmp-f-1251
                                       syntmp-id-1252
                                       syntmp-val-1253
                                       syntmp-e1-1254
                                       syntmp-e2-1255)
                                (syntmp-chi-let-1215
                                  syntmp-e-1229
                                  syntmp-r-1230
                                  syntmp-w-1231
                                  syntmp-s-1232
                                  syntmp-build-named-let-101
                                  (cons syntmp-f-1251 syntmp-id-1252)
                                  syntmp-val-1253
                                  (cons syntmp-e1-1254 syntmp-e2-1255)))
                              syntmp-tmp-1243)
                       ((lambda (syntmp-_-1259)
                          (syntax-error
                            (syntmp-source-wrap-147
                              syntmp-e-1229
                              syntmp-w-1231
                              syntmp-s-1232)))
                        syntmp-tmp-1233)))
                   (syntax-dispatch
                     syntmp-tmp-1233
                     '(any any #(each (any any)) any . each-any)))))
              (syntax-dispatch
                syntmp-tmp-1233
                '(any #(each (any any)) any . each-any))))
           syntmp-e-1229))))
    (syntmp-global-extend-116
      'core
      'letrec
      (lambda (syntmp-e-1260
               syntmp-r-1261
               syntmp-w-1262
               syntmp-s-1263)
        ((lambda (syntmp-tmp-1264)
           ((lambda (syntmp-tmp-1265)
              (if syntmp-tmp-1265
                (apply (lambda (syntmp-_-1266
                                syntmp-id-1267
                                syntmp-val-1268
                                syntmp-e1-1269
                                syntmp-e2-1270)
                         (let ((syntmp-ids-1271 syntmp-id-1267))
                           (if (not (syntmp-valid-bound-ids?-143
                                      syntmp-ids-1271))
                             (syntax-error
                               syntmp-e-1260
                               "duplicate bound variable in")
                             (let ((syntmp-labels-1273
                                     (syntmp-gen-labels-124 syntmp-ids-1271))
                                   (syntmp-new-vars-1274
                                     (map syntmp-gen-var-166 syntmp-ids-1271)))
                               (let ((syntmp-w-1275
                                       (syntmp-make-binding-wrap-135
                                         syntmp-ids-1271
                                         syntmp-labels-1273
                                         syntmp-w-1262))
                                     (syntmp-r-1276
                                       (syntmp-extend-var-env-113
                                         syntmp-labels-1273
                                         syntmp-new-vars-1274
                                         syntmp-r-1261)))
                                 (syntmp-build-letrec-102
                                   syntmp-s-1263
                                   syntmp-new-vars-1274
                                   (map (lambda (syntmp-x-1277)
                                          (syntmp-chi-154
                                            syntmp-x-1277
                                            syntmp-r-1276
                                            syntmp-w-1275))
                                        syntmp-val-1268)
                                   (syntmp-chi-body-158
                                     (cons syntmp-e1-1269 syntmp-e2-1270)
                                     (syntmp-source-wrap-147
                                       syntmp-e-1260
                                       syntmp-w-1275
                                       syntmp-s-1263)
                                     syntmp-r-1276
                                     syntmp-w-1275)))))))
                       syntmp-tmp-1265)
                ((lambda (syntmp-_-1280)
                   (syntax-error
                     (syntmp-source-wrap-147
                       syntmp-e-1260
                       syntmp-w-1262
                       syntmp-s-1263)))
                 syntmp-tmp-1264)))
            (syntax-dispatch
              syntmp-tmp-1264
              '(any #(each (any any)) any . each-any))))
         syntmp-e-1260)))
    (syntmp-global-extend-116
      'core
      'set!
      (lambda (syntmp-e-1281
               syntmp-r-1282
               syntmp-w-1283
               syntmp-s-1284)
        ((lambda (syntmp-tmp-1285)
           ((lambda (syntmp-tmp-1286)
              (if (if syntmp-tmp-1286
                    (apply (lambda (syntmp-_-1287
                                    syntmp-id-1288
                                    syntmp-val-1289)
                             (syntmp-id?-118 syntmp-id-1288))
                           syntmp-tmp-1286)
                    #f)
                (apply (lambda (syntmp-_-1290 syntmp-id-1291 syntmp-val-1292)
                         (let ((syntmp-val-1293
                                 (syntmp-chi-154
                                   syntmp-val-1292
                                   syntmp-r-1282
                                   syntmp-w-1283))
                               (syntmp-n-1294
                                 (syntmp-id-var-name-140
                                   syntmp-id-1291
                                   syntmp-w-1283)))
                           (let ((syntmp-b-1295
                                   (syntmp-lookup-115
                                     syntmp-n-1294
                                     syntmp-r-1282)))
                             (let ((syntmp-t-1296
                                     (syntmp-binding-type-110 syntmp-b-1295)))
                               (if (memv syntmp-t-1296 (quote (lexical)))
                                 (list 'set!
                                       (syntmp-binding-value-111 syntmp-b-1295)
                                       syntmp-val-1293)
                                 (if (memv syntmp-t-1296 (quote (global)))
                                   (list 'set!
                                         syntmp-n-1294
                                         syntmp-val-1293)
                                   (if (memv syntmp-t-1296
                                             '(displaced-lexical))
                                     (syntax-error
                                       (syntmp-wrap-146
                                         syntmp-id-1291
                                         syntmp-w-1283)
                                       "identifier out of context")
                                     (syntax-error
                                       (syntmp-source-wrap-147
                                         syntmp-e-1281
                                         syntmp-w-1283
                                         syntmp-s-1284)))))))))
                       syntmp-tmp-1286)
                ((lambda (syntmp-tmp-1297)
                   (if syntmp-tmp-1297
                     (apply (lambda (syntmp-_-1298
                                     syntmp-getter-1299
                                     syntmp-arg-1300
                                     syntmp-val-1301)
                              (cons (syntmp-chi-154
                                      (list '#(syntax-object
                                               setter
                                               ((top)
                                                #(ribcage
                                                  #(_ getter arg val)
                                                  #((top) (top) (top) (top))
                                                  #("i" "i" "i" "i"))
                                                #(ribcage () () ())
                                                #(ribcage
                                                  #(e r w s)
                                                  #((top) (top) (top) (top))
                                                  #("i" "i" "i" "i"))
                                                #(ribcage
                                                  (lambda-var-list
                                                    gen-var
                                                    strip
                                                    strip-annotation
                                                    ellipsis?
                                                    chi-void
                                                    eval-local-transformer
                                                    chi-local-syntax
                                                    chi-lambda-clause
                                                    chi-body
                                                    chi-macro
                                                    chi-application
                                                    chi-expr
                                                    chi
                                                    chi-top
                                                    syntax-type
                                                    chi-when-list
                                                    chi-install-global
                                                    chi-top-sequence
                                                    chi-sequence
                                                    source-wrap
                                                    wrap
                                                    bound-id-member?
                                                    distinct-bound-ids?
                                                    valid-bound-ids?
                                                    bound-id=?
                                                    free-id=?
                                                    id-var-name
                                                    same-marks?
                                                    join-marks
                                                    join-wraps
                                                    smart-append
                                                    make-binding-wrap
                                                    extend-ribcage!
                                                    make-empty-ribcage
                                                    new-mark
                                                    anti-mark
                                                    the-anti-mark
                                                    top-marked?
                                                    top-wrap
                                                    empty-wrap
                                                    set-ribcage-labels!
                                                    set-ribcage-marks!
                                                    set-ribcage-symnames!
                                                    ribcage-labels
                                                    ribcage-marks
                                                    ribcage-symnames
                                                    ribcage?
                                                    make-ribcage
                                                    gen-labels
                                                    gen-label
                                                    make-rename
                                                    rename-marks
                                                    rename-new
                                                    rename-old
                                                    subst-rename?
                                                    wrap-subst
                                                    wrap-marks
                                                    make-wrap
                                                    id-sym-name&marks
                                                    id-sym-name
                                                    id?
                                                    nonsymbol-id?
                                                    global-extend
                                                    lookup
                                                    macros-only-env
                                                    extend-var-env
                                                    extend-env
                                                    null-env
                                                    binding-value
                                                    binding-type
                                                    make-binding
                                                    arg-check
                                                    source-annotation
                                                    no-source
                                                    unannotate
                                                    set-syntax-object-wrap!
                                                    set-syntax-object-expression!
                                                    syntax-object-wrap
                                                    syntax-object-expression
                                                    syntax-object?
                                                    make-syntax-object
                                                    build-lexical-var
                                                    build-letrec
                                                    build-named-let
                                                    build-let
                                                    build-sequence
                                                    build-data
                                                    build-primref
                                                    build-lambda
                                                    build-global-definition
                                                    build-global-assignment
                                                    build-global-reference
                                                    build-lexical-assignment
                                                    build-lexical-reference
                                                    build-conditional
                                                    build-application
                                                    get-global-definition-hook
                                                    put-global-definition-hook
                                                    gensym-hook
                                                    error-hook
                                                    local-eval-hook
                                                    top-level-eval-hook
                                                    annotation?
                                                    fx<
                                                    fx=
                                                    fx-
                                                    fx+
                                                    noexpand)
                                                  ((top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top))
                                                  ("i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"
                                                   "i"))
                                                #(ribcage
                                                  (define-structure)
                                                  ((top))
                                                  ("i"))))
                                            syntmp-getter-1299)
                                      syntmp-r-1282
                                      syntmp-w-1283)
                                    (map (lambda (syntmp-e-1302)
                                           (syntmp-chi-154
                                             syntmp-e-1302
                                             syntmp-r-1282
                                             syntmp-w-1283))
                                         (append
                                           syntmp-arg-1300
                                           (list syntmp-val-1301)))))
                            syntmp-tmp-1297)
                     ((lambda (syntmp-_-1304)
                        (syntax-error
                          (syntmp-source-wrap-147
                            syntmp-e-1281
                            syntmp-w-1283
                            syntmp-s-1284)))
                      syntmp-tmp-1285)))
                 (syntax-dispatch
                   syntmp-tmp-1285
                   '(any (any . each-any) any)))))
            (syntax-dispatch
              syntmp-tmp-1285
              '(any any any))))
         syntmp-e-1281)))
    (syntmp-global-extend-116
      'begin
      'begin
      '())
    (syntmp-global-extend-116
      'define
      'define
      '())
    (syntmp-global-extend-116
      'define-syntax
      'define-syntax
      '())
    (syntmp-global-extend-116
      'eval-when
      'eval-when
      '())
    (syntmp-global-extend-116
      'core
      'syntax-case
      (letrec ((syntmp-gen-syntax-case-1308
                 (lambda (syntmp-x-1309
                          syntmp-keys-1310
                          syntmp-clauses-1311
                          syntmp-r-1312)
                   (if (null? syntmp-clauses-1311)
                     (list (quote syntax-error) syntmp-x-1309)
                     ((lambda (syntmp-tmp-1313)
                        ((lambda (syntmp-tmp-1314)
                           (if syntmp-tmp-1314
                             (apply (lambda (syntmp-pat-1315 syntmp-exp-1316)
                                      (if (and (syntmp-id?-118 syntmp-pat-1315)
                                               (andmap
                                                 (lambda (syntmp-x-1317)
                                                   (not (syntmp-free-id=?-141
                                                          syntmp-pat-1315
                                                          syntmp-x-1317)))
                                                 (cons '#(syntax-object
                                                          ...
                                                          ((top)
                                                           #(ribcage
                                                             #(pat exp)
                                                             #((top) (top))
                                                             #("i" "i"))
                                                           #(ribcage () () ())
                                                           #(ribcage
                                                             #(x
                                                               keys
                                                               clauses
                                                               r)
                                                             #((top)
                                                               (top)
                                                               (top)
                                                               (top))
                                                             #("i"
                                                               "i"
                                                               "i"
                                                               "i"))
                                                           #(ribcage
                                                             (gen-syntax-case
                                                               gen-clause
                                                               build-dispatch-call
                                                               convert-pattern)
                                                             ((top)
                                                              (top)
                                                              (top)
                                                              (top))
                                                             ("i" "i" "i" "i"))
                                                           #(ribcage
                                                             (lambda-var-list
                                                               gen-var
                                                               strip
                                                               strip-annotation
                                                               ellipsis?
                                                               chi-void
                                                               eval-local-transformer
                                                               chi-local-syntax
                                                               chi-lambda-clause
                                                               chi-body
                                                               chi-macro
                                                               chi-application
                                                               chi-expr
                                                               chi
                                                               chi-top
                                                               syntax-type
                                                               chi-when-list
                                                               chi-install-global
                                                               chi-top-sequence
                                                               chi-sequence
                                                               source-wrap
                                                               wrap
                                                               bound-id-member?
                                                               distinct-bound-ids?
                                                               valid-bound-ids?
                                                               bound-id=?
                                                               free-id=?
                                                               id-var-name
                                                               same-marks?
                                                               join-marks
                                                               join-wraps
                                                               smart-append
                                                               make-binding-wrap
                                                               extend-ribcage!
                                                               make-empty-ribcage
                                                               new-mark
                                                               anti-mark
                                                               the-anti-mark
                                                               top-marked?
                                                               top-wrap
                                                               empty-wrap
                                                               set-ribcage-labels!
                                                               set-ribcage-marks!
                                                               set-ribcage-symnames!
                                                               ribcage-labels
                                                               ribcage-marks
                                                               ribcage-symnames
                                                               ribcage?
                                                               make-ribcage
                                                               gen-labels
                                                               gen-label
                                                               make-rename
                                                               rename-marks
                                                               rename-new
                                                               rename-old
                                                               subst-rename?
                                                               wrap-subst
                                                               wrap-marks
                                                               make-wrap
                                                               id-sym-name&marks
                                                               id-sym-name
                                                               id?
                                                               nonsymbol-id?
                                                               global-extend
                                                               lookup
                                                               macros-only-env
                                                               extend-var-env
                                                               extend-env
                                                               null-env
                                                               binding-value
                                                               binding-type
                                                               make-binding
                                                               arg-check
                                                               source-annotation
                                                               no-source
                                                               unannotate
                                                               set-syntax-object-wrap!
                                                               set-syntax-object-expression!
                                                               syntax-object-wrap
                                                               syntax-object-expression
                                                               syntax-object?
                                                               make-syntax-object
                                                               build-lexical-var
                                                               build-letrec
                                                               build-named-let
                                                               build-let
                                                               build-sequence
                                                               build-data
                                                               build-primref
                                                               build-lambda
                                                               build-global-definition
                                                               build-global-assignment
                                                               build-global-reference
                                                               build-lexical-assignment
                                                               build-lexical-reference
                                                               build-conditional
                                                               build-application
                                                               get-global-definition-hook
                                                               put-global-definition-hook
                                                               gensym-hook
                                                               error-hook
                                                               local-eval-hook
                                                               top-level-eval-hook
                                                               annotation?
                                                               fx<
                                                               fx=
                                                               fx-
                                                               fx+
                                                               noexpand)
                                                             ((top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top))
                                                             ("i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"))
                                                           #(ribcage
                                                             (define-structure)
                                                             ((top))
                                                             ("i"))))
                                                       syntmp-keys-1310)))
                                        (let ((syntmp-labels-1318
                                                (list (syntmp-gen-label-123)))
                                              (syntmp-var-1319
                                                (syntmp-gen-var-166
                                                  syntmp-pat-1315)))
                                          (list (list 'lambda
                                                      (list syntmp-var-1319)
                                                      (syntmp-chi-154
                                                        syntmp-exp-1316
                                                        (syntmp-extend-env-112
                                                          syntmp-labels-1318
                                                          (list (cons 'syntax
                                                                      (cons syntmp-var-1319
                                                                            0)))
                                                          syntmp-r-1312)
                                                        (syntmp-make-binding-wrap-135
                                                          (list syntmp-pat-1315)
                                                          syntmp-labels-1318
                                                          '(()))))
                                                syntmp-x-1309))
                                        (syntmp-gen-clause-1307
                                          syntmp-x-1309
                                          syntmp-keys-1310
                                          (cdr syntmp-clauses-1311)
                                          syntmp-r-1312
                                          syntmp-pat-1315
                                          #t
                                          syntmp-exp-1316)))
                                    syntmp-tmp-1314)
                             ((lambda (syntmp-tmp-1320)
                                (if syntmp-tmp-1320
                                  (apply (lambda (syntmp-pat-1321
                                                  syntmp-fender-1322
                                                  syntmp-exp-1323)
                                           (syntmp-gen-clause-1307
                                             syntmp-x-1309
                                             syntmp-keys-1310
                                             (cdr syntmp-clauses-1311)
                                             syntmp-r-1312
                                             syntmp-pat-1321
                                             syntmp-fender-1322
                                             syntmp-exp-1323))
                                         syntmp-tmp-1320)
                                  ((lambda (syntmp-_-1324)
                                     (syntax-error
                                       (car syntmp-clauses-1311)
                                       "invalid syntax-case clause"))
                                   syntmp-tmp-1313)))
                              (syntax-dispatch
                                syntmp-tmp-1313
                                '(any any any)))))
                         (syntax-dispatch
                           syntmp-tmp-1313
                           '(any any))))
                      (car syntmp-clauses-1311)))))
               (syntmp-gen-clause-1307
                 (lambda (syntmp-x-1325
                          syntmp-keys-1326
                          syntmp-clauses-1327
                          syntmp-r-1328
                          syntmp-pat-1329
                          syntmp-fender-1330
                          syntmp-exp-1331)
                   (call-with-values
                     (lambda ()
                       (syntmp-convert-pattern-1305
                         syntmp-pat-1329
                         syntmp-keys-1326
                         (lambda (syntmp-e-1332)
                           (syntmp-ellipsis?-163
                             syntmp-e-1332
                             syntmp-r-1328))))
                     (lambda (syntmp-p-1333 syntmp-pvars-1334)
                       (cond ((not (syntmp-distinct-bound-ids?-144
                                     (map car syntmp-pvars-1334)))
                              (syntax-error
                                syntmp-pat-1329
                                "duplicate pattern variable in syntax-case pattern"))
                             ((not (andmap
                                     (lambda (syntmp-x-1335)
                                       (not (syntmp-ellipsis?-163
                                              (car syntmp-x-1335)
                                              syntmp-r-1328)))
                                     syntmp-pvars-1334))
                              (syntax-error
                                syntmp-pat-1329
                                "misplaced ellipsis in syntax-case pattern"))
                             (else
                              (let ((syntmp-y-1336
                                      (syntmp-gen-var-166 (quote tmp))))
                                (list (list 'lambda
                                            (list syntmp-y-1336)
                                            (let ((syntmp-y-1337
                                                    syntmp-y-1336))
                                              (list 'if
                                                    ((lambda (syntmp-tmp-1338)
                                                       ((lambda (syntmp-tmp-1339)
                                                          (if syntmp-tmp-1339
                                                            (apply (lambda ()
                                                                     syntmp-y-1337)
                                                                   syntmp-tmp-1339)
                                                            ((lambda (syntmp-_-1340)
                                                               (list 'if
                                                                     syntmp-y-1337
                                                                     (syntmp-build-dispatch-call-1306
                                                                       syntmp-pvars-1334
                                                                       syntmp-fender-1330
                                                                       syntmp-y-1337
                                                                       syntmp-r-1328)
                                                                     (syntmp-build-data-98
                                                                       #f
                                                                       #f)))
                                                             syntmp-tmp-1338)))
                                                        (syntax-dispatch
                                                          syntmp-tmp-1338
                                                          '#(atom #t))))
                                                     syntmp-fender-1330)
                                                    (syntmp-build-dispatch-call-1306
                                                      syntmp-pvars-1334
                                                      syntmp-exp-1331
                                                      syntmp-y-1337
                                                      syntmp-r-1328)
                                                    (syntmp-gen-syntax-case-1308
                                                      syntmp-x-1325
                                                      syntmp-keys-1326
                                                      syntmp-clauses-1327
                                                      syntmp-r-1328))))
                                      (if (eq? syntmp-p-1333 (quote any))
                                        (list (quote list) syntmp-x-1325)
                                        (list 'syntax-dispatch
                                              syntmp-x-1325
                                              (syntmp-build-data-98
                                                #f
                                                syntmp-p-1333)))))))))))
               (syntmp-build-dispatch-call-1306
                 (lambda (syntmp-pvars-1341
                          syntmp-exp-1342
                          syntmp-y-1343
                          syntmp-r-1344)
                   (let ((syntmp-ids-1345 (map car syntmp-pvars-1341))
                         (syntmp-levels-1346 (map cdr syntmp-pvars-1341)))
                     (let ((syntmp-labels-1347
                             (syntmp-gen-labels-124 syntmp-ids-1345))
                           (syntmp-new-vars-1348
                             (map syntmp-gen-var-166 syntmp-ids-1345)))
                       (list 'apply
                             (list 'lambda
                                   syntmp-new-vars-1348
                                   (syntmp-chi-154
                                     syntmp-exp-1342
                                     (syntmp-extend-env-112
                                       syntmp-labels-1347
                                       (map (lambda (syntmp-var-1349
                                                     syntmp-level-1350)
                                              (cons 'syntax
                                                    (cons syntmp-var-1349
                                                          syntmp-level-1350)))
                                            syntmp-new-vars-1348
                                            (map cdr syntmp-pvars-1341))
                                       syntmp-r-1344)
                                     (syntmp-make-binding-wrap-135
                                       syntmp-ids-1345
                                       syntmp-labels-1347
                                       '(()))))
                             syntmp-y-1343)))))
               (syntmp-convert-pattern-1305
                 (lambda (syntmp-pattern-1351
                          syntmp-keys-1352
                          syntmp-ellipsis?-1353)
                   (let syntmp-cvt-1354 ((syntmp-p-1355 syntmp-pattern-1351)
                                         (syntmp-n-1356 0)
                                         (syntmp-ids-1357 (quote ())))
                     (if (syntmp-id?-118 syntmp-p-1355)
                       (if (syntmp-bound-id-member?-145
                             syntmp-p-1355
                             syntmp-keys-1352)
                         (values
                           (vector (quote free-id) syntmp-p-1355)
                           syntmp-ids-1357)
                         (values
                           'any
                           (cons (cons syntmp-p-1355 syntmp-n-1356)
                                 syntmp-ids-1357)))
                       ((lambda (syntmp-tmp-1358)
                          ((lambda (syntmp-tmp-1359)
                             (if (if syntmp-tmp-1359
                                   (apply (lambda (syntmp-x-1360
                                                   syntmp-dots-1361)
                                            (syntmp-ellipsis?-1353
                                              syntmp-dots-1361))
                                          syntmp-tmp-1359)
                                   #f)
                               (apply (lambda (syntmp-x-1362 syntmp-dots-1363)
                                        (call-with-values
                                          (lambda ()
                                            (syntmp-cvt-1354
                                              syntmp-x-1362
                                              (syntmp-fx+-88 syntmp-n-1356 1)
                                              syntmp-ids-1357))
                                          (lambda (syntmp-p-1364
                                                   syntmp-ids-1365)
                                            (values
                                              (if (eq? syntmp-p-1364
                                                       'any)
                                                'each-any
                                                (vector
                                                  'each
                                                  syntmp-p-1364))
                                              syntmp-ids-1365))))
                                      syntmp-tmp-1359)
                               ((lambda (syntmp-tmp-1366)
                                  (if syntmp-tmp-1366
                                    (apply (lambda (syntmp-x-1367
                                                    syntmp-y-1368)
                                             (call-with-values
                                               (lambda ()
                                                 (syntmp-cvt-1354
                                                   syntmp-y-1368
                                                   syntmp-n-1356
                                                   syntmp-ids-1357))
                                               (lambda (syntmp-y-1369
                                                        syntmp-ids-1370)
                                                 (call-with-values
                                                   (lambda ()
                                                     (syntmp-cvt-1354
                                                       syntmp-x-1367
                                                       syntmp-n-1356
                                                       syntmp-ids-1370))
                                                   (lambda (syntmp-x-1371
                                                            syntmp-ids-1372)
                                                     (values
                                                       (cons syntmp-x-1371
                                                             syntmp-y-1369)
                                                       syntmp-ids-1372))))))
                                           syntmp-tmp-1366)
                                    ((lambda (syntmp-tmp-1373)
                                       (if syntmp-tmp-1373
                                         (apply (lambda ()
                                                  (values
                                                    '()
                                                    syntmp-ids-1357))
                                                syntmp-tmp-1373)
                                         ((lambda (syntmp-tmp-1374)
                                            (if syntmp-tmp-1374
                                              (apply (lambda (syntmp-x-1375)
                                                       (call-with-values
                                                         (lambda ()
                                                           (syntmp-cvt-1354
                                                             syntmp-x-1375
                                                             syntmp-n-1356
                                                             syntmp-ids-1357))
                                                         (lambda (syntmp-p-1377
                                                                  syntmp-ids-1378)
                                                           (values
                                                             (vector
                                                               'vector
                                                               syntmp-p-1377)
                                                             syntmp-ids-1378))))
                                                     syntmp-tmp-1374)
                                              ((lambda (syntmp-x-1379)
                                                 (values
                                                   (vector
                                                     'atom
                                                     (syntmp-strip-165
                                                       syntmp-p-1355
                                                       '(())))
                                                   syntmp-ids-1357))
                                               syntmp-tmp-1358)))
                                          (syntax-dispatch
                                            syntmp-tmp-1358
                                            '#(vector each-any)))))
                                     (syntax-dispatch
                                       syntmp-tmp-1358
                                       '()))))
                                (syntax-dispatch
                                  syntmp-tmp-1358
                                  '(any . any)))))
                           (syntax-dispatch
                             syntmp-tmp-1358
                             '(any any))))
                        syntmp-p-1355))))))
        (lambda (syntmp-e-1380
                 syntmp-r-1381
                 syntmp-w-1382
                 syntmp-s-1383)
          (let ((syntmp-e-1384
                  (syntmp-source-wrap-147
                    syntmp-e-1380
                    syntmp-w-1382
                    syntmp-s-1383)))
            ((lambda (syntmp-tmp-1385)
               ((lambda (syntmp-tmp-1386)
                  (if syntmp-tmp-1386
                    (apply (lambda (syntmp-_-1387
                                    syntmp-val-1388
                                    syntmp-key-1389
                                    syntmp-m-1390)
                             (if (andmap
                                   (lambda (syntmp-x-1391)
                                     (and (syntmp-id?-118 syntmp-x-1391)
                                          (not (syntmp-ellipsis?-163
                                                 syntmp-x-1391
                                                 syntmp-r-1381))))
                                   syntmp-key-1389)
                               (let ((syntmp-x-1393
                                       (syntmp-gen-var-166 (quote tmp))))
                                 (list (list 'lambda
                                             (list syntmp-x-1393)
                                             (syntmp-gen-syntax-case-1308
                                               syntmp-x-1393
                                               syntmp-key-1389
                                               syntmp-m-1390
                                               syntmp-r-1381))
                                       (syntmp-chi-154
                                         syntmp-val-1388
                                         syntmp-r-1381
                                         '(()))))
                               (syntax-error
                                 syntmp-e-1384
                                 "invalid literals list in")))
                           syntmp-tmp-1386)
                    (syntax-error syntmp-tmp-1385)))
                (syntax-dispatch
                  syntmp-tmp-1385
                  '(any any each-any . each-any))))
             syntmp-e-1384)))))
    (set! sc-expand
      (let ((syntmp-m-1396 (quote e))
            (syntmp-esew-1397 (quote (eval))))
        (lambda (syntmp-x-1398)
          (if (and (pair? syntmp-x-1398)
                   (equal? (car syntmp-x-1398) syntmp-noexpand-87))
            (cadr syntmp-x-1398)
            (syntmp-chi-top-153
              syntmp-x-1398
              '()
              '((top))
              syntmp-m-1396
              syntmp-esew-1397)))))
    (set! sc-expand3
      (let ((syntmp-m-1399 (quote e))
            (syntmp-esew-1400 (quote (eval))))
        (lambda (syntmp-x-1402 . syntmp-rest-1401)
          (if (and (pair? syntmp-x-1402)
                   (equal? (car syntmp-x-1402) syntmp-noexpand-87))
            (cadr syntmp-x-1402)
            (syntmp-chi-top-153
              syntmp-x-1402
              '()
              '((top))
              (if (null? syntmp-rest-1401)
                syntmp-m-1399
                (car syntmp-rest-1401))
              (if (or (null? syntmp-rest-1401)
                      (null? (cdr syntmp-rest-1401)))
                syntmp-esew-1400
                (cadr syntmp-rest-1401)))))))
    (set! identifier?
      (lambda (syntmp-x-1403)
        (syntmp-nonsymbol-id?-117 syntmp-x-1403)))
    (set! datum->syntax-object
      (lambda (syntmp-id-1404 syntmp-datum-1405)
        (syntmp-make-syntax-object-103
          syntmp-datum-1405
          (syntmp-syntax-object-wrap-106 syntmp-id-1404))))
    (set! syntax-object->datum
      (lambda (syntmp-x-1406)
        (syntmp-strip-165 syntmp-x-1406 (quote (())))))
    (set! generate-temporaries
      (lambda (syntmp-ls-1407)
        (begin
          (let ((syntmp-x-1408 syntmp-ls-1407))
            (if (not (list? syntmp-x-1408))
              (syntmp-error-hook-95
                'generate-temporaries
                "invalid argument"
                syntmp-x-1408)))
          (map (lambda (syntmp-x-1409)
                 (syntmp-wrap-146 (gensym) (quote ((top)))))
               syntmp-ls-1407))))
    (set! free-identifier=?
      (lambda (syntmp-x-1410 syntmp-y-1411)
        (begin
          (let ((syntmp-x-1412 syntmp-x-1410))
            (if (not (syntmp-nonsymbol-id?-117 syntmp-x-1412))
              (syntmp-error-hook-95
                'free-identifier=?
                "invalid argument"
                syntmp-x-1412)))
          (let ((syntmp-x-1413 syntmp-y-1411))
            (if (not (syntmp-nonsymbol-id?-117 syntmp-x-1413))
              (syntmp-error-hook-95
                'free-identifier=?
                "invalid argument"
                syntmp-x-1413)))
          (syntmp-free-id=?-141
            syntmp-x-1410
            syntmp-y-1411))))
    (set! bound-identifier=?
      (lambda (syntmp-x-1414 syntmp-y-1415)
        (begin
          (let ((syntmp-x-1416 syntmp-x-1414))
            (if (not (syntmp-nonsymbol-id?-117 syntmp-x-1416))
              (syntmp-error-hook-95
                'bound-identifier=?
                "invalid argument"
                syntmp-x-1416)))
          (let ((syntmp-x-1417 syntmp-y-1415))
            (if (not (syntmp-nonsymbol-id?-117 syntmp-x-1417))
              (syntmp-error-hook-95
                'bound-identifier=?
                "invalid argument"
                syntmp-x-1417)))
          (syntmp-bound-id=?-142
            syntmp-x-1414
            syntmp-y-1415))))
    (set! syntax-error
      (lambda (syntmp-object-1419 . syntmp-messages-1418)
        (begin
          (for-each
            (lambda (syntmp-x-1420)
              (let ((syntmp-x-1421 syntmp-x-1420))
                (if (not (string? syntmp-x-1421))
                  (syntmp-error-hook-95
                    'syntax-error
                    "invalid argument"
                    syntmp-x-1421))))
            syntmp-messages-1418)
          (let ((syntmp-message-1422
                  (if (null? syntmp-messages-1418)
                    "invalid syntax"
                    (apply string-append syntmp-messages-1418))))
            (syntmp-error-hook-95
              #f
              syntmp-message-1422
              (syntmp-strip-165
                syntmp-object-1419
                '(())))))))
    (set! install-global-transformer
      (lambda (syntmp-sym-1423 syntmp-v-1424)
        (begin
          (let ((syntmp-x-1425 syntmp-sym-1423))
            (if (not (symbol? syntmp-x-1425))
              (syntmp-error-hook-95
                'define-syntax
                "invalid argument"
                syntmp-x-1425)))
          (let ((syntmp-x-1426 syntmp-v-1424))
            (if (not (procedure? syntmp-x-1426))
              (syntmp-error-hook-95
                'define-syntax
                "invalid argument"
                syntmp-x-1426)))
          (syntmp-global-extend-116
            'macro
            syntmp-sym-1423
            syntmp-v-1424))))
    (letrec ((syntmp-match-1431
               (lambda (syntmp-e-1432
                        syntmp-p-1433
                        syntmp-w-1434
                        syntmp-r-1435)
                 (cond ((not syntmp-r-1435) #f)
                       ((eq? syntmp-p-1433 (quote _)) syntmp-r-1435)
                       ((eq? syntmp-p-1433 (quote any))
                        (cons (syntmp-wrap-146 syntmp-e-1432 syntmp-w-1434)
                              syntmp-r-1435))
                       ((syntmp-syntax-object?-104 syntmp-e-1432)
                        (syntmp-match*-1430
                          (let ((syntmp-e-1436
                                  (syntmp-syntax-object-expression-105
                                    syntmp-e-1432)))
                            (if (syntmp-annotation?-92 syntmp-e-1436)
                              (annotation-expression syntmp-e-1436)
                              syntmp-e-1436))
                          syntmp-p-1433
                          (syntmp-join-wraps-137
                            syntmp-w-1434
                            (syntmp-syntax-object-wrap-106 syntmp-e-1432))
                          syntmp-r-1435))
                       (else
                        (syntmp-match*-1430
                          (let ((syntmp-e-1437 syntmp-e-1432))
                            (if (syntmp-annotation?-92 syntmp-e-1437)
                              (annotation-expression syntmp-e-1437)
                              syntmp-e-1437))
                          syntmp-p-1433
                          syntmp-w-1434
                          syntmp-r-1435)))))
             (syntmp-match*-1430
               (lambda (syntmp-e-1438
                        syntmp-p-1439
                        syntmp-w-1440
                        syntmp-r-1441)
                 (cond ((null? syntmp-p-1439)
                        (and (null? syntmp-e-1438) syntmp-r-1441))
                       ((pair? syntmp-p-1439)
                        (and (pair? syntmp-e-1438)
                             (syntmp-match-1431
                               (car syntmp-e-1438)
                               (car syntmp-p-1439)
                               syntmp-w-1440
                               (syntmp-match-1431
                                 (cdr syntmp-e-1438)
                                 (cdr syntmp-p-1439)
                                 syntmp-w-1440
                                 syntmp-r-1441))))
                       ((eq? syntmp-p-1439 (quote each-any))
                        (let ((syntmp-l-1442
                                (syntmp-match-each-any-1428
                                  syntmp-e-1438
                                  syntmp-w-1440)))
                          (and syntmp-l-1442
                               (cons syntmp-l-1442 syntmp-r-1441))))
                       (else
                        (let ((syntmp-t-1443 (vector-ref syntmp-p-1439 0)))
                          (if (memv syntmp-t-1443 (quote (each)))
                            (if (null? syntmp-e-1438)
                              (syntmp-match-empty-1429
                                (vector-ref syntmp-p-1439 1)
                                syntmp-r-1441)
                              (let ((syntmp-l-1444
                                      (syntmp-match-each-1427
                                        syntmp-e-1438
                                        (vector-ref syntmp-p-1439 1)
                                        syntmp-w-1440)))
                                (and syntmp-l-1444
                                     (let syntmp-collect-1445 ((syntmp-l-1446
                                                                 syntmp-l-1444))
                                       (if (null? (car syntmp-l-1446))
                                         syntmp-r-1441
                                         (cons (map car syntmp-l-1446)
                                               (syntmp-collect-1445
                                                 (map cdr syntmp-l-1446))))))))
                            (if (memv syntmp-t-1443 (quote (free-id)))
                              (and (syntmp-id?-118 syntmp-e-1438)
                                   (syntmp-free-id=?-141
                                     (syntmp-wrap-146
                                       syntmp-e-1438
                                       syntmp-w-1440)
                                     (vector-ref syntmp-p-1439 1))
                                   syntmp-r-1441)
                              (if (memv syntmp-t-1443 (quote (atom)))
                                (and (equal?
                                       (vector-ref syntmp-p-1439 1)
                                       (syntmp-strip-165
                                         syntmp-e-1438
                                         syntmp-w-1440))
                                     syntmp-r-1441)
                                (if (memv syntmp-t-1443 (quote (vector)))
                                  (and (vector? syntmp-e-1438)
                                       (syntmp-match-1431
                                         (vector->list syntmp-e-1438)
                                         (vector-ref syntmp-p-1439 1)
                                         syntmp-w-1440
                                         syntmp-r-1441)))))))))))
             (syntmp-match-empty-1429
               (lambda (syntmp-p-1447 syntmp-r-1448)
                 (cond ((null? syntmp-p-1447) syntmp-r-1448)
                       ((eq? syntmp-p-1447 (quote _)) syntmp-r-1448)
                       ((eq? syntmp-p-1447 (quote any))
                        (cons (quote ()) syntmp-r-1448))
                       ((pair? syntmp-p-1447)
                        (syntmp-match-empty-1429
                          (car syntmp-p-1447)
                          (syntmp-match-empty-1429
                            (cdr syntmp-p-1447)
                            syntmp-r-1448)))
                       ((eq? syntmp-p-1447 (quote each-any))
                        (cons (quote ()) syntmp-r-1448))
                       (else
                        (let ((syntmp-t-1449 (vector-ref syntmp-p-1447 0)))
                          (if (memv syntmp-t-1449 (quote (each)))
                            (syntmp-match-empty-1429
                              (vector-ref syntmp-p-1447 1)
                              syntmp-r-1448)
                            (if (memv syntmp-t-1449 (quote (free-id atom)))
                              syntmp-r-1448
                              (if (memv syntmp-t-1449 (quote (vector)))
                                (syntmp-match-empty-1429
                                  (vector-ref syntmp-p-1447 1)
                                  syntmp-r-1448)))))))))
             (syntmp-match-each-any-1428
               (lambda (syntmp-e-1450 syntmp-w-1451)
                 (cond ((syntmp-annotation?-92 syntmp-e-1450)
                        (syntmp-match-each-any-1428
                          (annotation-expression syntmp-e-1450)
                          syntmp-w-1451))
                       ((pair? syntmp-e-1450)
                        (let ((syntmp-l-1452
                                (syntmp-match-each-any-1428
                                  (cdr syntmp-e-1450)
                                  syntmp-w-1451)))
                          (and syntmp-l-1452
                               (cons (syntmp-wrap-146
                                       (car syntmp-e-1450)
                                       syntmp-w-1451)
                                     syntmp-l-1452))))
                       ((null? syntmp-e-1450) (quote ()))
                       ((syntmp-syntax-object?-104 syntmp-e-1450)
                        (syntmp-match-each-any-1428
                          (syntmp-syntax-object-expression-105
                            syntmp-e-1450)
                          (syntmp-join-wraps-137
                            syntmp-w-1451
                            (syntmp-syntax-object-wrap-106 syntmp-e-1450))))
                       (else #f))))
             (syntmp-match-each-1427
               (lambda (syntmp-e-1453 syntmp-p-1454 syntmp-w-1455)
                 (cond ((syntmp-annotation?-92 syntmp-e-1453)
                        (syntmp-match-each-1427
                          (annotation-expression syntmp-e-1453)
                          syntmp-p-1454
                          syntmp-w-1455))
                       ((pair? syntmp-e-1453)
                        (let ((syntmp-first-1456
                                (syntmp-match-1431
                                  (car syntmp-e-1453)
                                  syntmp-p-1454
                                  syntmp-w-1455
                                  '())))
                          (and syntmp-first-1456
                               (let ((syntmp-rest-1457
                                       (syntmp-match-each-1427
                                         (cdr syntmp-e-1453)
                                         syntmp-p-1454
                                         syntmp-w-1455)))
                                 (and syntmp-rest-1457
                                      (cons syntmp-first-1456
                                            syntmp-rest-1457))))))
                       ((null? syntmp-e-1453) (quote ()))
                       ((syntmp-syntax-object?-104 syntmp-e-1453)
                        (syntmp-match-each-1427
                          (syntmp-syntax-object-expression-105
                            syntmp-e-1453)
                          syntmp-p-1454
                          (syntmp-join-wraps-137
                            syntmp-w-1455
                            (syntmp-syntax-object-wrap-106 syntmp-e-1453))))
                       (else #f)))))
      (begin
        (set! syntax-dispatch
          (lambda (syntmp-e-1458 syntmp-p-1459)
            (cond ((eq? syntmp-p-1459 (quote any))
                   (list syntmp-e-1458))
                  ((eq? syntmp-p-1459 (quote _)) (quote ()))
                  ((syntmp-syntax-object?-104 syntmp-e-1458)
                   (syntmp-match*-1430
                     (let ((syntmp-e-1460
                             (syntmp-syntax-object-expression-105
                               syntmp-e-1458)))
                       (if (syntmp-annotation?-92 syntmp-e-1460)
                         (annotation-expression syntmp-e-1460)
                         syntmp-e-1460))
                     syntmp-p-1459
                     (syntmp-syntax-object-wrap-106 syntmp-e-1458)
                     '()))
                  (else
                   (syntmp-match*-1430
                     (let ((syntmp-e-1461 syntmp-e-1458))
                       (if (syntmp-annotation?-92 syntmp-e-1461)
                         (annotation-expression syntmp-e-1461)
                         syntmp-e-1461))
                     syntmp-p-1459
                     '(())
                     '())))))
        (set! sc-chi syntmp-chi-154)))))

(install-global-transformer
  'with-syntax
  (lambda (syntmp-x-1462)
    ((lambda (syntmp-tmp-1463)
       ((lambda (syntmp-tmp-1464)
          (if syntmp-tmp-1464
            (apply (lambda (syntmp-_-1465 syntmp-e1-1466 syntmp-e2-1467)
                     (cons '#(syntax-object
                              let
                              ((top)
                               #(ribcage
                                 #(_ e1 e2)
                                 #((top) (top) (top))
                                 #("i" "i" "i"))
                               #(ribcage () () ())
                               #(ribcage #(x) #((top)) #("i"))))
                           (cons '()
                                 (cons syntmp-e1-1466 syntmp-e2-1467))))
                   syntmp-tmp-1464)
            ((lambda (syntmp-tmp-1469)
               (if syntmp-tmp-1469
                 (apply (lambda (syntmp-_-1470
                                 syntmp-out-1471
                                 syntmp-in-1472
                                 syntmp-e1-1473
                                 syntmp-e2-1474)
                          (list '#(syntax-object
                                   syntax-case
                                   ((top)
                                    #(ribcage
                                      #(_ out in e1 e2)
                                      #((top) (top) (top) (top) (top))
                                      #("i" "i" "i" "i" "i"))
                                    #(ribcage () () ())
                                    #(ribcage #(x) #((top)) #("i"))))
                                syntmp-in-1472
                                '()
                                (list syntmp-out-1471
                                      (cons '#(syntax-object
                                               let
                                               ((top)
                                                #(ribcage
                                                  #(_ out in e1 e2)
                                                  #((top)
                                                    (top)
                                                    (top)
                                                    (top)
                                                    (top))
                                                  #("i" "i" "i" "i" "i"))
                                                #(ribcage () () ())
                                                #(ribcage
                                                  #(x)
                                                  #((top))
                                                  #("i"))))
                                            (cons '()
                                                  (cons syntmp-e1-1473
                                                        syntmp-e2-1474))))))
                        syntmp-tmp-1469)
                 ((lambda (syntmp-tmp-1476)
                    (if syntmp-tmp-1476
                      (apply (lambda (syntmp-_-1477
                                      syntmp-out-1478
                                      syntmp-in-1479
                                      syntmp-e1-1480
                                      syntmp-e2-1481)
                               (list '#(syntax-object
                                        syntax-case
                                        ((top)
                                         #(ribcage
                                           #(_ out in e1 e2)
                                           #((top) (top) (top) (top) (top))
                                           #("i" "i" "i" "i" "i"))
                                         #(ribcage () () ())
                                         #(ribcage #(x) #((top)) #("i"))))
                                     (cons '#(syntax-object
                                              list
                                              ((top)
                                               #(ribcage
                                                 #(_ out in e1 e2)
                                                 #((top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top))
                                                 #("i" "i" "i" "i" "i"))
                                               #(ribcage () () ())
                                               #(ribcage
                                                 #(x)
                                                 #((top))
                                                 #("i"))))
                                           syntmp-in-1479)
                                     '()
                                     (list syntmp-out-1478
                                           (cons '#(syntax-object
                                                    let
                                                    ((top)
                                                     #(ribcage
                                                       #(_ out in e1 e2)
                                                       #((top)
                                                         (top)
                                                         (top)
                                                         (top)
                                                         (top))
                                                       #("i" "i" "i" "i" "i"))
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(x)
                                                       #((top))
                                                       #("i"))))
                                                 (cons '()
                                                       (cons syntmp-e1-1480
                                                             syntmp-e2-1481))))))
                             syntmp-tmp-1476)
                      (syntax-error syntmp-tmp-1463)))
                  (syntax-dispatch
                    syntmp-tmp-1463
                    '(any #(each (any any)) any . each-any)))))
             (syntax-dispatch
               syntmp-tmp-1463
               '(any ((any any)) any . each-any)))))
        (syntax-dispatch
          syntmp-tmp-1463
          '(any () any . each-any))))
     syntmp-x-1462)))

(install-global-transformer
  'syntax-rules
  (lambda (syntmp-xx-1503)
    (letrec ((syntmp-expand-syntax-rules-1504
               (lambda (syntmp-dots-1505
                        syntmp-keys-1506
                        syntmp-docstrings-1507
                        syntmp-clauses-1508)
                 ((lambda (syntmp-tmp-1509)
                    ((lambda (syntmp-tmp-1510)
                       (if syntmp-tmp-1510
                         (apply (lambda (syntmp-k-1511
                                         syntmp-docstring-1512
                                         syntmp-keyword-1513
                                         syntmp-pattern-1514
                                         syntmp-template-1515)
                                  ((lambda (syntmp-tmp-1516)
                                     ((lambda (syntmp-form-1517)
                                        (if syntmp-dots-1505
                                          ((lambda (syntmp-tmp-1518)
                                             ((lambda (syntmp-dots-1519)
                                                (list '#(syntax-object
                                                         with-ellipsis
                                                         ((top)
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(dots)
                                                            #((top))
                                                            #("i"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(form)
                                                            #((top))
                                                            #("i"))
                                                          #(ribcage () () ())
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(k
                                                              docstring
                                                              keyword
                                                              pattern
                                                              template)
                                                            #((top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top))
                                                            #("i"
                                                              "i"
                                                              "i"
                                                              "i"
                                                              "i"))
                                                          #(ribcage () () ())
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(dots
                                                              keys
                                                              docstrings
                                                              clauses)
                                                            #((top)
                                                              (top)
                                                              (top)
                                                              (top))
                                                            #("i" "i" "i" "i"))
                                                          #(ribcage
                                                            (expand-syntax-rules)
                                                            ((top))
                                                            ("i"))
                                                          #(ribcage
                                                            #(xx)
                                                            #((top))
                                                            #("i"))))
                                                      syntmp-dots-1519
                                                      syntmp-form-1517))
                                              syntmp-tmp-1518))
                                           syntmp-dots-1505)
                                          syntmp-form-1517))
                                      syntmp-tmp-1516))
                                   (cons '#(syntax-object
                                            lambda
                                            ((top)
                                             #(ribcage () () ())
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(k
                                                 docstring
                                                 keyword
                                                 pattern
                                                 template)
                                               #((top) (top) (top) (top) (top))
                                               #("i" "i" "i" "i" "i"))
                                             #(ribcage () () ())
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(dots keys docstrings clauses)
                                               #((top) (top) (top) (top))
                                               #("i" "i" "i" "i"))
                                             #(ribcage
                                               (expand-syntax-rules)
                                               ((top))
                                               ("i"))
                                             #(ribcage #(xx) #((top)) #("i"))))
                                         (cons '(#(syntax-object
                                                   x
                                                   ((top)
                                                    #(ribcage () () ())
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(k
                                                        docstring
                                                        keyword
                                                        pattern
                                                        template)
                                                      #((top)
                                                        (top)
                                                        (top)
                                                        (top)
                                                        (top))
                                                      #("i" "i" "i" "i" "i"))
                                                    #(ribcage () () ())
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(dots
                                                        keys
                                                        docstrings
                                                        clauses)
                                                      #((top)
                                                        (top)
                                                        (top)
                                                        (top))
                                                      #("i" "i" "i" "i"))
                                                    #(ribcage
                                                      (expand-syntax-rules)
                                                      ((top))
                                                      ("i"))
                                                    #(ribcage
                                                      #(xx)
                                                      #((top))
                                                      #("i")))))
                                               (append
                                                 syntmp-docstring-1512
                                                 (list (cons '#(syntax-object
                                                                syntax-case
                                                                ((top)
                                                                 #(ribcage
                                                                   ()
                                                                   ()
                                                                   ())
                                                                 #(ribcage
                                                                   ()
                                                                   ()
                                                                   ())
                                                                 #(ribcage
                                                                   #(k
                                                                     docstring
                                                                     keyword
                                                                     pattern
                                                                     template)
                                                                   #((top)
                                                                     (top)
                                                                     (top)
                                                                     (top)
                                                                     (top))
                                                                   #("i"
                                                                     "i"
                                                                     "i"
                                                                     "i"
                                                                     "i"))
                                                                 #(ribcage
                                                                   ()
                                                                   ()
                                                                   ())
                                                                 #(ribcage
                                                                   ()
                                                                   ()
                                                                   ())
                                                                 #(ribcage
                                                                   #(dots
                                                                     keys
                                                                     docstrings
                                                                     clauses)
                                                                   #((top)
                                                                     (top)
                                                                     (top)
                                                                     (top))
                                                                   #("i"
                                                                     "i"
                                                                     "i"
                                                                     "i"))
                                                                 #(ribcage
                                                                   (expand-syntax-rules)
                                                                   ((top))
                                                                   ("i"))
                                                                 #(ribcage
                                                                   #(xx)
                                                                   #((top))
                                                                   #("i"))))
                                                             (cons '#(syntax-object
                                                                      x
                                                                      ((top)
                                                                       #(ribcage
                                                                         ()
                                                                         ()
                                                                         ())
                                                                       #(ribcage
                                                                         ()
                                                                         ()
                                                                         ())
                                                                       #(ribcage
                                                                         #(k
                                                                           docstring
                                                                           keyword
                                                                           pattern
                                                                           template)
                                                                         #((top)
                                                                           (top)
                                                                           (top)
                                                                           (top)
                                                                           (top))
                                                                         #("i"
                                                                           "i"
                                                                           "i"
                                                                           "i"
                                                                           "i"))
                                                                       #(ribcage
                                                                         ()
                                                                         ()
                                                                         ())
                                                                       #(ribcage
                                                                         ()
                                                                         ()
                                                                         ())
                                                                       #(ribcage
                                                                         #(dots
                                                                           keys
                                                                           docstrings
                                                                           clauses)
                                                                         #((top)
                                                                           (top)
                                                                           (top)
                                                                           (top))
                                                                         #("i"
                                                                           "i"
                                                                           "i"
                                                                           "i"))
                                                                       #(ribcage
                                                                         (expand-syntax-rules)
                                                                         ((top))
                                                                         ("i"))
                                                                       #(ribcage
                                                                         #(xx)
                                                                         #((top))
                                                                         #("i"))))
                                                                   (cons syntmp-k-1511
                                                                         (map (lambda (syntmp-tmp-1522
                                                                                       syntmp-tmp-1521)
                                                                                (list (cons '#(syntax-object
                                                                                               dummy
                                                                                               ((top)
                                                                                                #(ribcage
                                                                                                  ()
                                                                                                  ()
                                                                                                  ())
                                                                                                #(ribcage
                                                                                                  ()
                                                                                                  ()
                                                                                                  ())
                                                                                                #(ribcage
                                                                                                  #(k
                                                                                                    docstring
                                                                                                    keyword
                                                                                                    pattern
                                                                                                    template)
                                                                                                  #((top)
                                                                                                    (top)
                                                                                                    (top)
                                                                                                    (top)
                                                                                                    (top))
                                                                                                  #("i"
                                                                                                    "i"
                                                                                                    "i"
                                                                                                    "i"
                                                                                                    "i"))
                                                                                                #(ribcage
                                                                                                  ()
                                                                                                  ()
                                                                                                  ())
                                                                                                #(ribcage
                                                                                                  ()
                                                                                                  ()
                                                                                                  ())
                                                                                                #(ribcage
                                                                                                  #(dots
                                                                                                    keys
                                                                                                    docstrings
                                                                                                    clauses)
                                                                                                  #((top)
                                                                                                    (top)
                                                                                                    (top)
                                                                                                    (top))
                                                                                                  #("i"
                                                                                                    "i"
                                                                                                    "i"
                                                                                                    "i"))
                                                                                                #(ribcage
                                                                                                  (expand-syntax-rules)
                                                                                                  ((top))
                                                                                                  ("i"))
                                                                                                #(ribcage
                                                                                                  #(xx)
                                                                                                  #((top))
                                                                                                  #("i"))))
                                                                                            syntmp-tmp-1521)
                                                                                      (list '#(syntax-object
                                                                                               syntax
                                                                                               ((top)
                                                                                                #(ribcage
                                                                                                  ()
                                                                                                  ()
                                                                                                  ())
                                                                                                #(ribcage
                                                                                                  ()
                                                                                                  ()
                                                                                                  ())
                                                                                                #(ribcage
                                                                                                  #(k
                                                                                                    docstring
                                                                                                    keyword
                                                                                                    pattern
                                                                                                    template)
                                                                                                  #((top)
                                                                                                    (top)
                                                                                                    (top)
                                                                                                    (top)
                                                                                                    (top))
                                                                                                  #("i"
                                                                                                    "i"
                                                                                                    "i"
                                                                                                    "i"
                                                                                                    "i"))
                                                                                                #(ribcage
                                                                                                  ()
                                                                                                  ()
                                                                                                  ())
                                                                                                #(ribcage
                                                                                                  ()
                                                                                                  ()
                                                                                                  ())
                                                                                                #(ribcage
                                                                                                  #(dots
                                                                                                    keys
                                                                                                    docstrings
                                                                                                    clauses)
                                                                                                  #((top)
                                                                                                    (top)
                                                                                                    (top)
                                                                                                    (top))
                                                                                                  #("i"
                                                                                                    "i"
                                                                                                    "i"
                                                                                                    "i"))
                                                                                                #(ribcage
                                                                                                  (expand-syntax-rules)
                                                                                                  ((top))
                                                                                                  ("i"))
                                                                                                #(ribcage
                                                                                                  #(xx)
                                                                                                  #((top))
                                                                                                  #("i"))))
                                                                                            syntmp-tmp-1522)))
                                                                              syntmp-template-1515
                                                                              syntmp-pattern-1514))))))))))
                                syntmp-tmp-1510)
                         (syntax-error syntmp-tmp-1509)))
                     (syntax-dispatch
                       syntmp-tmp-1509
                       '(each-any each-any #(each ((any . any) any))))))
                  (list syntmp-keys-1506
                        syntmp-docstrings-1507
                        syntmp-clauses-1508)))))
      ((lambda (syntmp-tmp-1524)
         ((lambda (syntmp-tmp-1525)
            (if syntmp-tmp-1525
              (apply (lambda (syntmp-_-1526
                              syntmp-k-1527
                              syntmp-keyword-1528
                              syntmp-pattern-1529
                              syntmp-template-1530)
                       (syntmp-expand-syntax-rules-1504
                         #f
                         syntmp-k-1527
                         '()
                         (map (lambda (syntmp-tmp-1534
                                       syntmp-tmp-1533
                                       syntmp-tmp-1532)
                                (list (cons syntmp-tmp-1532 syntmp-tmp-1533)
                                      syntmp-tmp-1534))
                              syntmp-template-1530
                              syntmp-pattern-1529
                              syntmp-keyword-1528)))
                     syntmp-tmp-1525)
              ((lambda (syntmp-tmp-1535)
                 (if (if syntmp-tmp-1535
                       (apply (lambda (syntmp-_-1536
                                       syntmp-k-1537
                                       syntmp-docstring-1538
                                       syntmp-keyword-1539
                                       syntmp-pattern-1540
                                       syntmp-template-1541)
                                (string?
                                  (syntax-object->datum
                                    syntmp-docstring-1538)))
                              syntmp-tmp-1535)
                       #f)
                   (apply (lambda (syntmp-_-1542
                                   syntmp-k-1543
                                   syntmp-docstring-1544
                                   syntmp-keyword-1545
                                   syntmp-pattern-1546
                                   syntmp-template-1547)
                            (syntmp-expand-syntax-rules-1504
                              #f
                              syntmp-k-1543
                              (list syntmp-docstring-1544)
                              (map (lambda (syntmp-tmp-1551
                                            syntmp-tmp-1550
                                            syntmp-tmp-1549)
                                     (list (cons syntmp-tmp-1549
                                                 syntmp-tmp-1550)
                                           syntmp-tmp-1551))
                                   syntmp-template-1547
                                   syntmp-pattern-1546
                                   syntmp-keyword-1545)))
                          syntmp-tmp-1535)
                   ((lambda (syntmp-tmp-1552)
                      (if (if syntmp-tmp-1552
                            (apply (lambda (syntmp-_-1553
                                            syntmp-dots-1554
                                            syntmp-k-1555
                                            syntmp-keyword-1556
                                            syntmp-pattern-1557
                                            syntmp-template-1558)
                                     (identifier? syntmp-dots-1554))
                                   syntmp-tmp-1552)
                            #f)
                        (apply (lambda (syntmp-_-1559
                                        syntmp-dots-1560
                                        syntmp-k-1561
                                        syntmp-keyword-1562
                                        syntmp-pattern-1563
                                        syntmp-template-1564)
                                 (syntmp-expand-syntax-rules-1504
                                   syntmp-dots-1560
                                   syntmp-k-1561
                                   '()
                                   (map (lambda (syntmp-tmp-1568
                                                 syntmp-tmp-1567
                                                 syntmp-tmp-1566)
                                          (list (cons syntmp-tmp-1566
                                                      syntmp-tmp-1567)
                                                syntmp-tmp-1568))
                                        syntmp-template-1564
                                        syntmp-pattern-1563
                                        syntmp-keyword-1562)))
                               syntmp-tmp-1552)
                        ((lambda (syntmp-tmp-1569)
                           (if (if syntmp-tmp-1569
                                 (apply (lambda (syntmp-_-1570
                                                 syntmp-dots-1571
                                                 syntmp-k-1572
                                                 syntmp-docstring-1573
                                                 syntmp-keyword-1574
                                                 syntmp-pattern-1575
                                                 syntmp-template-1576)
                                          (and (identifier? syntmp-dots-1571)
                                               (string?
                                                 (syntax-object->datum
                                                   syntmp-docstring-1573))))
                                        syntmp-tmp-1569)
                                 #f)
                             (apply (lambda (syntmp-_-1577
                                             syntmp-dots-1578
                                             syntmp-k-1579
                                             syntmp-docstring-1580
                                             syntmp-keyword-1581
                                             syntmp-pattern-1582
                                             syntmp-template-1583)
                                      (syntmp-expand-syntax-rules-1504
                                        syntmp-dots-1578
                                        syntmp-k-1579
                                        (list syntmp-docstring-1580)
                                        (map (lambda (syntmp-tmp-1587
                                                      syntmp-tmp-1586
                                                      syntmp-tmp-1585)
                                               (list (cons syntmp-tmp-1585
                                                           syntmp-tmp-1586)
                                                     syntmp-tmp-1587))
                                             syntmp-template-1583
                                             syntmp-pattern-1582
                                             syntmp-keyword-1581)))
                                    syntmp-tmp-1569)
                             (syntax-error syntmp-tmp-1524)))
                         (syntax-dispatch
                           syntmp-tmp-1524
                           '(any any
                                 each-any
                                 any
                                 .
                                 #(each ((any . any) any)))))))
                    (syntax-dispatch
                      syntmp-tmp-1524
                      '(any any each-any . #(each ((any . any) any)))))))
               (syntax-dispatch
                 syntmp-tmp-1524
                 '(any each-any any . #(each ((any . any) any)))))))
          (syntax-dispatch
            syntmp-tmp-1524
            '(any each-any . #(each ((any . any) any))))))
       syntmp-xx-1503))))

(install-global-transformer
  'define-syntax-rule
  (lambda (syntmp-x-1667)
    ((lambda (syntmp-tmp-1668)
       ((lambda (syntmp-tmp-1669)
          (if syntmp-tmp-1669
            (apply (lambda (syntmp-_-1670
                            syntmp-name-1671
                            syntmp-pattern-1672
                            syntmp-template-1673)
                     (list '#(syntax-object
                              define-syntax
                              ((top)
                               #(ribcage
                                 #(_ name pattern template)
                                 #((top) (top) (top) (top))
                                 #("i" "i" "i" "i"))
                               #(ribcage () () ())
                               #(ribcage #(x) #((top)) #("i"))))
                           syntmp-name-1671
                           (list '#(syntax-object
                                    syntax-rules
                                    ((top)
                                     #(ribcage
                                       #(_ name pattern template)
                                       #((top) (top) (top) (top))
                                       #("i" "i" "i" "i"))
                                     #(ribcage () () ())
                                     #(ribcage #(x) #((top)) #("i"))))
                                 '()
                                 (list (cons syntmp-_-1670 syntmp-pattern-1672)
                                       syntmp-template-1673))))
                   syntmp-tmp-1669)
            ((lambda (syntmp-tmp-1674)
               (if (if syntmp-tmp-1674
                     (apply (lambda (syntmp-_-1675
                                     syntmp-name-1676
                                     syntmp-pattern-1677
                                     syntmp-docstring-1678
                                     syntmp-template-1679)
                              (string?
                                (syntax-object->datum syntmp-docstring-1678)))
                            syntmp-tmp-1674)
                     #f)
                 (apply (lambda (syntmp-_-1680
                                 syntmp-name-1681
                                 syntmp-pattern-1682
                                 syntmp-docstring-1683
                                 syntmp-template-1684)
                          (list '#(syntax-object
                                   define-syntax
                                   ((top)
                                    #(ribcage
                                      #(_ name pattern docstring template)
                                      #((top) (top) (top) (top) (top))
                                      #("i" "i" "i" "i" "i"))
                                    #(ribcage () () ())
                                    #(ribcage #(x) #((top)) #("i"))))
                                syntmp-name-1681
                                (list '#(syntax-object
                                         syntax-rules
                                         ((top)
                                          #(ribcage
                                            #(_
                                              name
                                              pattern
                                              docstring
                                              template)
                                            #((top) (top) (top) (top) (top))
                                            #("i" "i" "i" "i" "i"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i"))))
                                      '()
                                      syntmp-docstring-1683
                                      (list (cons syntmp-_-1680
                                                  syntmp-pattern-1682)
                                            syntmp-template-1684))))
                        syntmp-tmp-1674)
                 (syntax-error syntmp-tmp-1668)))
             (syntax-dispatch
               syntmp-tmp-1668
               '(any (any . any) any any)))))
        (syntax-dispatch
          syntmp-tmp-1668
          '(any (any . any) any))))
     syntmp-x-1667)))

(install-global-transformer
  'let*
  (lambda (syntmp-x-1703)
    ((lambda (syntmp-tmp-1704)
       ((lambda (syntmp-tmp-1705)
          (if (if syntmp-tmp-1705
                (apply (lambda (syntmp-let*-1706
                                syntmp-x-1707
                                syntmp-v-1708
                                syntmp-e1-1709
                                syntmp-e2-1710)
                         (andmap identifier? syntmp-x-1707))
                       syntmp-tmp-1705)
                #f)
            (apply (lambda (syntmp-let*-1712
                            syntmp-x-1713
                            syntmp-v-1714
                            syntmp-e1-1715
                            syntmp-e2-1716)
                     (let syntmp-f-1717 ((syntmp-bindings-1718
                                           (map list
                                                syntmp-x-1713
                                                syntmp-v-1714)))
                       (if (null? syntmp-bindings-1718)
                         (cons '#(syntax-object
                                  let
                                  ((top)
                                   #(ribcage () () ())
                                   #(ribcage
                                     #(f bindings)
                                     #((top) (top))
                                     #("i" "i"))
                                   #(ribcage
                                     #(let* x v e1 e2)
                                     #((top) (top) (top) (top) (top))
                                     #("i" "i" "i" "i" "i"))
                                   #(ribcage () () ())
                                   #(ribcage #(x) #((top)) #("i"))))
                               (cons '()
                                     (cons syntmp-e1-1715 syntmp-e2-1716)))
                         ((lambda (syntmp-tmp-1722)
                            ((lambda (syntmp-tmp-1723)
                               (if syntmp-tmp-1723
                                 (apply (lambda (syntmp-body-1724
                                                 syntmp-binding-1725)
                                          (list '#(syntax-object
                                                   let
                                                   ((top)
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(body binding)
                                                      #((top) (top))
                                                      #("i" "i"))
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(f bindings)
                                                      #((top) (top))
                                                      #("i" "i"))
                                                    #(ribcage
                                                      #(let* x v e1 e2)
                                                      #((top)
                                                        (top)
                                                        (top)
                                                        (top)
                                                        (top))
                                                      #("i" "i" "i" "i" "i"))
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(x)
                                                      #((top))
                                                      #("i"))))
                                                (list syntmp-binding-1725)
                                                syntmp-body-1724))
                                        syntmp-tmp-1723)
                                 (syntax-error syntmp-tmp-1722)))
                             (syntax-dispatch
                               syntmp-tmp-1722
                               '(any any))))
                          (list (syntmp-f-1717 (cdr syntmp-bindings-1718))
                                (car syntmp-bindings-1718))))))
                   syntmp-tmp-1705)
            (syntax-error syntmp-tmp-1704)))
        (syntax-dispatch
          syntmp-tmp-1704
          '(any #(each (any any)) any . each-any))))
     syntmp-x-1703)))

(install-global-transformer
  'do
  (lambda (syntmp-orig-x-1745)
    ((lambda (syntmp-tmp-1746)
       ((lambda (syntmp-tmp-1747)
          (if syntmp-tmp-1747
            (apply (lambda (syntmp-_-1748
                            syntmp-var-1749
                            syntmp-init-1750
                            syntmp-step-1751
                            syntmp-e0-1752
                            syntmp-e1-1753
                            syntmp-c-1754)
                     ((lambda (syntmp-tmp-1755)
                        ((lambda (syntmp-tmp-1756)
                           (if syntmp-tmp-1756
                             (apply (lambda (syntmp-step-1757)
                                      ((lambda (syntmp-tmp-1758)
                                         ((lambda (syntmp-tmp-1759)
                                            (if syntmp-tmp-1759
                                              (apply (lambda ()
                                                       (list '#(syntax-object
                                                                let
                                                                ((top)
                                                                 #(ribcage
                                                                   ()
                                                                   ()
                                                                   ())
                                                                 #(ribcage
                                                                   #(step)
                                                                   #((top))
                                                                   #("i"))
                                                                 #(ribcage
                                                                   #(_
                                                                     var
                                                                     init
                                                                     step
                                                                     e0
                                                                     e1
                                                                     c)
                                                                   #((top)
                                                                     (top)
                                                                     (top)
                                                                     (top)
                                                                     (top)
                                                                     (top)
                                                                     (top))
                                                                   #("i"
                                                                     "i"
                                                                     "i"
                                                                     "i"
                                                                     "i"
                                                                     "i"
                                                                     "i"))
                                                                 #(ribcage
                                                                   ()
                                                                   ()
                                                                   ())
                                                                 #(ribcage
                                                                   #(orig-x)
                                                                   #((top))
                                                                   #("i"))))
                                                             '#(syntax-object
                                                                doloop
                                                                ((top)
                                                                 #(ribcage
                                                                   ()
                                                                   ()
                                                                   ())
                                                                 #(ribcage
                                                                   #(step)
                                                                   #((top))
                                                                   #("i"))
                                                                 #(ribcage
                                                                   #(_
                                                                     var
                                                                     init
                                                                     step
                                                                     e0
                                                                     e1
                                                                     c)
                                                                   #((top)
                                                                     (top)
                                                                     (top)
                                                                     (top)
                                                                     (top)
                                                                     (top)
                                                                     (top))
                                                                   #("i"
                                                                     "i"
                                                                     "i"
                                                                     "i"
                                                                     "i"
                                                                     "i"
                                                                     "i"))
                                                                 #(ribcage
                                                                   ()
                                                                   ()
                                                                   ())
                                                                 #(ribcage
                                                                   #(orig-x)
                                                                   #((top))
                                                                   #("i"))))
                                                             (map list
                                                                  syntmp-var-1749
                                                                  syntmp-init-1750)
                                                             (list '#(syntax-object
                                                                      if
                                                                      ((top)
                                                                       #(ribcage
                                                                         ()
                                                                         ()
                                                                         ())
                                                                       #(ribcage
                                                                         #(step)
                                                                         #((top))
                                                                         #("i"))
                                                                       #(ribcage
                                                                         #(_
                                                                           var
                                                                           init
                                                                           step
                                                                           e0
                                                                           e1
                                                                           c)
                                                                         #((top)
                                                                           (top)
                                                                           (top)
                                                                           (top)
                                                                           (top)
                                                                           (top)
                                                                           (top))
                                                                         #("i"
                                                                           "i"
                                                                           "i"
                                                                           "i"
                                                                           "i"
                                                                           "i"
                                                                           "i"))
                                                                       #(ribcage
                                                                         ()
                                                                         ()
                                                                         ())
                                                                       #(ribcage
                                                                         #(orig-x)
                                                                         #((top))
                                                                         #("i"))))
                                                                   (list '#(syntax-object
                                                                            not
                                                                            ((top)
                                                                             #(ribcage
                                                                               ()
                                                                               ()
                                                                               ())
                                                                             #(ribcage
                                                                               #(step)
                                                                               #((top))
                                                                               #("i"))
                                                                             #(ribcage
                                                                               #(_
                                                                                 var
                                                                                 init
                                                                                 step
                                                                                 e0
                                                                                 e1
                                                                                 c)
                                                                               #((top)
                                                                                 (top)
                                                                                 (top)
                                                                                 (top)
                                                                                 (top)
                                                                                 (top)
                                                                                 (top))
                                                                               #("i"
                                                                                 "i"
                                                                                 "i"
                                                                                 "i"
                                                                                 "i"
                                                                                 "i"
                                                                                 "i"))
                                                                             #(ribcage
                                                                               ()
                                                                               ()
                                                                               ())
                                                                             #(ribcage
                                                                               #(orig-x)
                                                                               #((top))
                                                                               #("i"))))
                                                                         syntmp-e0-1752)
                                                                   (cons '#(syntax-object
                                                                            begin
                                                                            ((top)
                                                                             #(ribcage
                                                                               ()
                                                                               ()
                                                                               ())
                                                                             #(ribcage
                                                                               #(step)
                                                                               #((top))
                                                                               #("i"))
                                                                             #(ribcage
                                                                               #(_
                                                                                 var
                                                                                 init
                                                                                 step
                                                                                 e0
                                                                                 e1
                                                                                 c)
                                                                               #((top)
                                                                                 (top)
                                                                                 (top)
                                                                                 (top)
                                                                                 (top)
                                                                                 (top)
                                                                                 (top))
                                                                               #("i"
                                                                                 "i"
                                                                                 "i"
                                                                                 "i"
                                                                                 "i"
                                                                                 "i"
                                                                                 "i"))
                                                                             #(ribcage
                                                                               ()
                                                                               ()
                                                                               ())
                                                                             #(ribcage
                                                                               #(orig-x)
                                                                               #((top))
                                                                               #("i"))))
                                                                         (append
                                                                           syntmp-c-1754
                                                                           (list (cons '#(syntax-object
                                                                                          doloop
                                                                                          ((top)
                                                                                           #(ribcage
                                                                                             ()
                                                                                             ()
                                                                                             ())
                                                                                           #(ribcage
                                                                                             #(step)
                                                                                             #((top))
                                                                                             #("i"))
                                                                                           #(ribcage
                                                                                             #(_
                                                                                               var
                                                                                               init
                                                                                               step
                                                                                               e0
                                                                                               e1
                                                                                               c)
                                                                                             #((top)
                                                                                               (top)
                                                                                               (top)
                                                                                               (top)
                                                                                               (top)
                                                                                               (top)
                                                                                               (top))
                                                                                             #("i"
                                                                                               "i"
                                                                                               "i"
                                                                                               "i"
                                                                                               "i"
                                                                                               "i"
                                                                                               "i"))
                                                                                           #(ribcage
                                                                                             ()
                                                                                             ()
                                                                                             ())
                                                                                           #(ribcage
                                                                                             #(orig-x)
                                                                                             #((top))
                                                                                             #("i"))))
                                                                                       syntmp-step-1757)))))))
                                                     syntmp-tmp-1759)
                                              ((lambda (syntmp-tmp-1764)
                                                 (if syntmp-tmp-1764
                                                   (apply (lambda (syntmp-e1-1765
                                                                   syntmp-e2-1766)
                                                            (list '#(syntax-object
                                                                     let
                                                                     ((top)
                                                                      #(ribcage
                                                                        #(e1
                                                                          e2)
                                                                        #((top)
                                                                          (top))
                                                                        #("i"
                                                                          "i"))
                                                                      #(ribcage
                                                                        ()
                                                                        ()
                                                                        ())
                                                                      #(ribcage
                                                                        #(step)
                                                                        #((top))
                                                                        #("i"))
                                                                      #(ribcage
                                                                        #(_
                                                                          var
                                                                          init
                                                                          step
                                                                          e0
                                                                          e1
                                                                          c)
                                                                        #((top)
                                                                          (top)
                                                                          (top)
                                                                          (top)
                                                                          (top)
                                                                          (top)
                                                                          (top))
                                                                        #("i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"))
                                                                      #(ribcage
                                                                        ()
                                                                        ()
                                                                        ())
                                                                      #(ribcage
                                                                        #(orig-x)
                                                                        #((top))
                                                                        #("i"))))
                                                                  '#(syntax-object
                                                                     doloop
                                                                     ((top)
                                                                      #(ribcage
                                                                        #(e1
                                                                          e2)
                                                                        #((top)
                                                                          (top))
                                                                        #("i"
                                                                          "i"))
                                                                      #(ribcage
                                                                        ()
                                                                        ()
                                                                        ())
                                                                      #(ribcage
                                                                        #(step)
                                                                        #((top))
                                                                        #("i"))
                                                                      #(ribcage
                                                                        #(_
                                                                          var
                                                                          init
                                                                          step
                                                                          e0
                                                                          e1
                                                                          c)
                                                                        #((top)
                                                                          (top)
                                                                          (top)
                                                                          (top)
                                                                          (top)
                                                                          (top)
                                                                          (top))
                                                                        #("i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"
                                                                          "i"))
                                                                      #(ribcage
                                                                        ()
                                                                        ()
                                                                        ())
                                                                      #(ribcage
                                                                        #(orig-x)
                                                                        #((top))
                                                                        #("i"))))
                                                                  (map list
                                                                       syntmp-var-1749
                                                                       syntmp-init-1750)
                                                                  (list '#(syntax-object
                                                                           if
                                                                           ((top)
                                                                            #(ribcage
                                                                              #(e1
                                                                                e2)
                                                                              #((top)
                                                                                (top))
                                                                              #("i"
                                                                                "i"))
                                                                            #(ribcage
                                                                              ()
                                                                              ()
                                                                              ())
                                                                            #(ribcage
                                                                              #(step)
                                                                              #((top))
                                                                              #("i"))
                                                                            #(ribcage
                                                                              #(_
                                                                                var
                                                                                init
                                                                                step
                                                                                e0
                                                                                e1
                                                                                c)
                                                                              #((top)
                                                                                (top)
                                                                                (top)
                                                                                (top)
                                                                                (top)
                                                                                (top)
                                                                                (top))
                                                                              #("i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"
                                                                                "i"))
                                                                            #(ribcage
                                                                              ()
                                                                              ()
                                                                              ())
                                                                            #(ribcage
                                                                              #(orig-x)
                                                                              #((top))
                                                                              #("i"))))
                                                                        syntmp-e0-1752
                                                                        (cons '#(syntax-object
                                                                                 begin
                                                                                 ((top)
                                                                                  #(ribcage
                                                                                    #(e1
                                                                                      e2)
                                                                                    #((top)
                                                                                      (top))
                                                                                    #("i"
                                                                                      "i"))
                                                                                  #(ribcage
                                                                                    ()
                                                                                    ()
                                                                                    ())
                                                                                  #(ribcage
                                                                                    #(step)
                                                                                    #((top))
                                                                                    #("i"))
                                                                                  #(ribcage
                                                                                    #(_
                                                                                      var
                                                                                      init
                                                                                      step
                                                                                      e0
                                                                                      e1
                                                                                      c)
                                                                                    #((top)
                                                                                      (top)
                                                                                      (top)
                                                                                      (top)
                                                                                      (top)
                                                                                      (top)
                                                                                      (top))
                                                                                    #("i"
                                                                                      "i"
                                                                                      "i"
                                                                                      "i"
                                                                                      "i"
                                                                                      "i"
                                                                                      "i"))
                                                                                  #(ribcage
                                                                                    ()
                                                                                    ()
                                                                                    ())
                                                                                  #(ribcage
                                                                                    #(orig-x)
                                                                                    #((top))
                                                                                    #("i"))))
                                                                              (cons syntmp-e1-1765
                                                                                    syntmp-e2-1766))
                                                                        (cons '#(syntax-object
                                                                                 begin
                                                                                 ((top)
                                                                                  #(ribcage
                                                                                    #(e1
                                                                                      e2)
                                                                                    #((top)
                                                                                      (top))
                                                                                    #("i"
                                                                                      "i"))
                                                                                  #(ribcage
                                                                                    ()
                                                                                    ()
                                                                                    ())
                                                                                  #(ribcage
                                                                                    #(step)
                                                                                    #((top))
                                                                                    #("i"))
                                                                                  #(ribcage
                                                                                    #(_
                                                                                      var
                                                                                      init
                                                                                      step
                                                                                      e0
                                                                                      e1
                                                                                      c)
                                                                                    #((top)
                                                                                      (top)
                                                                                      (top)
                                                                                      (top)
                                                                                      (top)
                                                                                      (top)
                                                                                      (top))
                                                                                    #("i"
                                                                                      "i"
                                                                                      "i"
                                                                                      "i"
                                                                                      "i"
                                                                                      "i"
                                                                                      "i"))
                                                                                  #(ribcage
                                                                                    ()
                                                                                    ()
                                                                                    ())
                                                                                  #(ribcage
                                                                                    #(orig-x)
                                                                                    #((top))
                                                                                    #("i"))))
                                                                              (append
                                                                                syntmp-c-1754
                                                                                (list (cons '#(syntax-object
                                                                                               doloop
                                                                                               ((top)
                                                                                                #(ribcage
                                                                                                  #(e1
                                                                                                    e2)
                                                                                                  #((top)
                                                                                                    (top))
                                                                                                  #("i"
                                                                                                    "i"))
                                                                                                #(ribcage
                                                                                                  ()
                                                                                                  ()
                                                                                                  ())
                                                                                                #(ribcage
                                                                                                  #(step)
                                                                                                  #((top))
                                                                                                  #("i"))
                                                                                                #(ribcage
                                                                                                  #(_
                                                                                                    var
                                                                                                    init
                                                                                                    step
                                                                                                    e0
                                                                                                    e1
                                                                                                    c)
                                                                                                  #((top)
                                                                                                    (top)
                                                                                                    (top)
                                                                                                    (top)
                                                                                                    (top)
                                                                                                    (top)
                                                                                                    (top))
                                                                                                  #("i"
                                                                                                    "i"
                                                                                                    "i"
                                                                                                    "i"
                                                                                                    "i"
                                                                                                    "i"
                                                                                                    "i"))
                                                                                                #(ribcage
                                                                                                  ()
                                                                                                  ()
                                                                                                  ())
                                                                                                #(ribcage
                                                                                                  #(orig-x)
                                                                                                  #((top))
                                                                                                  #("i"))))
                                                                                            syntmp-step-1757)))))))
                                                          syntmp-tmp-1764)
                                                   (syntax-error
                                                     syntmp-tmp-1758)))
                                               (syntax-dispatch
                                                 syntmp-tmp-1758
                                                 '(any . each-any)))))
                                          (syntax-dispatch
                                            syntmp-tmp-1758
                                            '())))
                                       syntmp-e1-1753))
                                    syntmp-tmp-1756)
                             (syntax-error syntmp-tmp-1755)))
                         (syntax-dispatch
                           syntmp-tmp-1755
                           'each-any)))
                      (map (lambda (syntmp-v-1773 syntmp-s-1774)
                             ((lambda (syntmp-tmp-1775)
                                ((lambda (syntmp-tmp-1776)
                                   (if syntmp-tmp-1776
                                     (apply (lambda () syntmp-v-1773)
                                            syntmp-tmp-1776)
                                     ((lambda (syntmp-tmp-1777)
                                        (if syntmp-tmp-1777
                                          (apply (lambda (syntmp-e-1778)
                                                   syntmp-e-1778)
                                                 syntmp-tmp-1777)
                                          ((lambda (syntmp-_-1779)
                                             (syntax-error syntmp-orig-x-1745))
                                           syntmp-tmp-1775)))
                                      (syntax-dispatch
                                        syntmp-tmp-1775
                                        '(any)))))
                                 (syntax-dispatch syntmp-tmp-1775 (quote ()))))
                              syntmp-s-1774))
                           syntmp-var-1749
                           syntmp-step-1751)))
                   syntmp-tmp-1747)
            (syntax-error syntmp-tmp-1746)))
        (syntax-dispatch
          syntmp-tmp-1746
          '(any #(each (any any . any))
                (any . each-any)
                .
                each-any))))
     syntmp-orig-x-1745)))

(install-global-transformer
  'quasiquote
  (letrec ((syntmp-quasicons-1807
             (lambda (syntmp-x-1811 syntmp-y-1812)
               ((lambda (syntmp-tmp-1813)
                  ((lambda (syntmp-tmp-1814)
                     (if syntmp-tmp-1814
                       (apply (lambda (syntmp-x-1815 syntmp-y-1816)
                                ((lambda (syntmp-tmp-1817)
                                   ((lambda (syntmp-tmp-1818)
                                      (if syntmp-tmp-1818
                                        (apply (lambda (syntmp-dy-1819)
                                                 ((lambda (syntmp-tmp-1820)
                                                    ((lambda (syntmp-tmp-1821)
                                                       (if syntmp-tmp-1821
                                                         (apply (lambda (syntmp-dx-1822)
                                                                  (list '#(syntax-object
                                                                           quote
                                                                           ((top)
                                                                            #(ribcage
                                                                              #(dx)
                                                                              #((top))
                                                                              #("i"))
                                                                            #(ribcage
                                                                              #(dy)
                                                                              #((top))
                                                                              #("i"))
                                                                            #(ribcage
                                                                              ()
                                                                              ()
                                                                              ())
                                                                            #(ribcage
                                                                              #(x
                                                                                y)
                                                                              #((top)
                                                                                (top))
                                                                              #("i"
                                                                                "i"))
                                                                            #(ribcage
                                                                              ()
                                                                              ()
                                                                              ())
                                                                            #(ribcage
                                                                              ()
                                                                              ()
                                                                              ())
                                                                            #(ribcage
                                                                              #(x
                                                                                y)
                                                                              #((top)
                                                                                (top))
                                                                              #("i"
                                                                                "i"))
                                                                            #(ribcage
                                                                              #(quasicons
                                                                                quasiappend
                                                                                quasivector
                                                                                quasi)
                                                                              #((top)
                                                                                (top)
                                                                                (top)
                                                                                (top))
                                                                              #("i"
                                                                                "i"
                                                                                "i"
                                                                                "i"))))
                                                                        (cons syntmp-dx-1822
                                                                              syntmp-dy-1819)))
                                                                syntmp-tmp-1821)
                                                         ((lambda (syntmp-_-1823)
                                                            (if (null? syntmp-dy-1819)
                                                              (list '#(syntax-object
                                                                       list
                                                                       ((top)
                                                                        #(ribcage
                                                                          #(_)
                                                                          #((top))
                                                                          #("i"))
                                                                        #(ribcage
                                                                          #(dy)
                                                                          #((top))
                                                                          #("i"))
                                                                        #(ribcage
                                                                          ()
                                                                          ()
                                                                          ())
                                                                        #(ribcage
                                                                          #(x
                                                                            y)
                                                                          #((top)
                                                                            (top))
                                                                          #("i"
                                                                            "i"))
                                                                        #(ribcage
                                                                          ()
                                                                          ()
                                                                          ())
                                                                        #(ribcage
                                                                          ()
                                                                          ()
                                                                          ())
                                                                        #(ribcage
                                                                          #(x
                                                                            y)
                                                                          #((top)
                                                                            (top))
                                                                          #("i"
                                                                            "i"))
                                                                        #(ribcage
                                                                          #(quasicons
                                                                            quasiappend
                                                                            quasivector
                                                                            quasi)
                                                                          #((top)
                                                                            (top)
                                                                            (top)
                                                                            (top))
                                                                          #("i"
                                                                            "i"
                                                                            "i"
                                                                            "i"))))
                                                                    syntmp-x-1815)
                                                              (list '#(syntax-object
                                                                       cons
                                                                       ((top)
                                                                        #(ribcage
                                                                          #(_)
                                                                          #((top))
                                                                          #("i"))
                                                                        #(ribcage
                                                                          #(dy)
                                                                          #((top))
                                                                          #("i"))
                                                                        #(ribcage
                                                                          ()
                                                                          ()
                                                                          ())
                                                                        #(ribcage
                                                                          #(x
                                                                            y)
                                                                          #((top)
                                                                            (top))
                                                                          #("i"
                                                                            "i"))
                                                                        #(ribcage
                                                                          ()
                                                                          ()
                                                                          ())
                                                                        #(ribcage
                                                                          ()
                                                                          ()
                                                                          ())
                                                                        #(ribcage
                                                                          #(x
                                                                            y)
                                                                          #((top)
                                                                            (top))
                                                                          #("i"
                                                                            "i"))
                                                                        #(ribcage
                                                                          #(quasicons
                                                                            quasiappend
                                                                            quasivector
                                                                            quasi)
                                                                          #((top)
                                                                            (top)
                                                                            (top)
                                                                            (top))
                                                                          #("i"
                                                                            "i"
                                                                            "i"
                                                                            "i"))))
                                                                    syntmp-x-1815
                                                                    syntmp-y-1816)))
                                                          syntmp-tmp-1820)))
                                                     (syntax-dispatch
                                                       syntmp-tmp-1820
                                                       '(#(free-id
                                                           #(syntax-object
                                                             quote
                                                             ((top)
                                                              #(ribcage
                                                                #(dy)
                                                                #((top))
                                                                #("i"))
                                                              #(ribcage
                                                                ()
                                                                ()
                                                                ())
                                                              #(ribcage
                                                                #(x y)
                                                                #((top) (top))
                                                                #("i" "i"))
                                                              #(ribcage
                                                                ()
                                                                ()
                                                                ())
                                                              #(ribcage
                                                                ()
                                                                ()
                                                                ())
                                                              #(ribcage
                                                                #(x y)
                                                                #((top) (top))
                                                                #("i" "i"))
                                                              #(ribcage
                                                                #(quasicons
                                                                  quasiappend
                                                                  quasivector
                                                                  quasi)
                                                                #((top)
                                                                  (top)
                                                                  (top)
                                                                  (top))
                                                                #("i"
                                                                  "i"
                                                                  "i"
                                                                  "i")))))
                                                         any))))
                                                  syntmp-x-1815))
                                               syntmp-tmp-1818)
                                        ((lambda (syntmp-tmp-1824)
                                           (if syntmp-tmp-1824
                                             (apply (lambda (syntmp-stuff-1825)
                                                      (cons '#(syntax-object
                                                               list
                                                               ((top)
                                                                #(ribcage
                                                                  #(stuff)
                                                                  #((top))
                                                                  #("i"))
                                                                #(ribcage
                                                                  ()
                                                                  ()
                                                                  ())
                                                                #(ribcage
                                                                  #(x y)
                                                                  #((top)
                                                                    (top))
                                                                  #("i" "i"))
                                                                #(ribcage
                                                                  ()
                                                                  ()
                                                                  ())
                                                                #(ribcage
                                                                  ()
                                                                  ()
                                                                  ())
                                                                #(ribcage
                                                                  #(x y)
                                                                  #((top)
                                                                    (top))
                                                                  #("i" "i"))
                                                                #(ribcage
                                                                  #(quasicons
                                                                    quasiappend
                                                                    quasivector
                                                                    quasi)
                                                                  #((top)
                                                                    (top)
                                                                    (top)
                                                                    (top))
                                                                  #("i"
                                                                    "i"
                                                                    "i"
                                                                    "i"))))
                                                            (cons syntmp-x-1815
                                                                  syntmp-stuff-1825)))
                                                    syntmp-tmp-1824)
                                             ((lambda (syntmp-else-1826)
                                                (list '#(syntax-object
                                                         cons
                                                         ((top)
                                                          #(ribcage
                                                            #(else)
                                                            #((top))
                                                            #("i"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(x y)
                                                            #((top) (top))
                                                            #("i" "i"))
                                                          #(ribcage () () ())
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(x y)
                                                            #((top) (top))
                                                            #("i" "i"))
                                                          #(ribcage
                                                            #(quasicons
                                                              quasiappend
                                                              quasivector
                                                              quasi)
                                                            #((top)
                                                              (top)
                                                              (top)
                                                              (top))
                                                            #("i"
                                                              "i"
                                                              "i"
                                                              "i"))))
                                                      syntmp-x-1815
                                                      syntmp-y-1816))
                                              syntmp-tmp-1817)))
                                         (syntax-dispatch
                                           syntmp-tmp-1817
                                           '(#(free-id
                                               #(syntax-object
                                                 list
                                                 ((top)
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(x y)
                                                    #((top) (top))
                                                    #("i" "i"))
                                                  #(ribcage () () ())
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(x y)
                                                    #((top) (top))
                                                    #("i" "i"))
                                                  #(ribcage
                                                    #(quasicons
                                                      quasiappend
                                                      quasivector
                                                      quasi)
                                                    #((top) (top) (top) (top))
                                                    #("i" "i" "i" "i")))))
                                             .
                                             any)))))
                                    (syntax-dispatch
                                      syntmp-tmp-1817
                                      '(#(free-id
                                          #(syntax-object
                                            quote
                                            ((top)
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x y)
                                               #((top) (top))
                                               #("i" "i"))
                                             #(ribcage () () ())
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x y)
                                               #((top) (top))
                                               #("i" "i"))
                                             #(ribcage
                                               #(quasicons
                                                 quasiappend
                                                 quasivector
                                                 quasi)
                                               #((top) (top) (top) (top))
                                               #("i" "i" "i" "i")))))
                                        any))))
                                 syntmp-y-1816))
                              syntmp-tmp-1814)
                       (syntax-error syntmp-tmp-1813)))
                   (syntax-dispatch
                     syntmp-tmp-1813
                     '(any any))))
                (list syntmp-x-1811 syntmp-y-1812))))
           (syntmp-quasiappend-1808
             (lambda (syntmp-x-1827 syntmp-y-1828)
               ((lambda (syntmp-tmp-1829)
                  ((lambda (syntmp-tmp-1830)
                     (if syntmp-tmp-1830
                       (apply (lambda (syntmp-x-1831 syntmp-y-1832)
                                ((lambda (syntmp-tmp-1833)
                                   ((lambda (syntmp-tmp-1834)
                                      (if syntmp-tmp-1834
                                        (apply (lambda () syntmp-x-1831)
                                               syntmp-tmp-1834)
                                        ((lambda (syntmp-_-1835)
                                           (list '#(syntax-object
                                                    append
                                                    ((top)
                                                     #(ribcage
                                                       #(_)
                                                       #((top))
                                                       #("i"))
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(x y)
                                                       #((top) (top))
                                                       #("i" "i"))
                                                     #(ribcage () () ())
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(x y)
                                                       #((top) (top))
                                                       #("i" "i"))
                                                     #(ribcage
                                                       #(quasicons
                                                         quasiappend
                                                         quasivector
                                                         quasi)
                                                       #((top)
                                                         (top)
                                                         (top)
                                                         (top))
                                                       #("i" "i" "i" "i"))))
                                                 syntmp-x-1831
                                                 syntmp-y-1832))
                                         syntmp-tmp-1833)))
                                    (syntax-dispatch
                                      syntmp-tmp-1833
                                      '(#(free-id
                                          #(syntax-object
                                            quote
                                            ((top)
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x y)
                                               #((top) (top))
                                               #("i" "i"))
                                             #(ribcage () () ())
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x y)
                                               #((top) (top))
                                               #("i" "i"))
                                             #(ribcage
                                               #(quasicons
                                                 quasiappend
                                                 quasivector
                                                 quasi)
                                               #((top) (top) (top) (top))
                                               #("i" "i" "i" "i")))))
                                        ()))))
                                 syntmp-y-1832))
                              syntmp-tmp-1830)
                       (syntax-error syntmp-tmp-1829)))
                   (syntax-dispatch
                     syntmp-tmp-1829
                     '(any any))))
                (list syntmp-x-1827 syntmp-y-1828))))
           (syntmp-quasivector-1809
             (lambda (syntmp-x-1836)
               ((lambda (syntmp-tmp-1837)
                  ((lambda (syntmp-x-1838)
                     ((lambda (syntmp-tmp-1839)
                        ((lambda (syntmp-tmp-1840)
                           (if syntmp-tmp-1840
                             (apply (lambda (syntmp-x-1841)
                                      (list '#(syntax-object
                                               quote
                                               ((top)
                                                #(ribcage #(x) #((top)) #("i"))
                                                #(ribcage () () ())
                                                #(ribcage #(x) #((top)) #("i"))
                                                #(ribcage () () ())
                                                #(ribcage () () ())
                                                #(ribcage #(x) #((top)) #("i"))
                                                #(ribcage
                                                  #(quasicons
                                                    quasiappend
                                                    quasivector
                                                    quasi)
                                                  #((top) (top) (top) (top))
                                                  #("i" "i" "i" "i"))))
                                            (list->vector syntmp-x-1841)))
                                    syntmp-tmp-1840)
                             ((lambda (syntmp-tmp-1843)
                                (if syntmp-tmp-1843
                                  (apply (lambda (syntmp-x-1844)
                                           (cons '#(syntax-object
                                                    vector
                                                    ((top)
                                                     #(ribcage
                                                       #(x)
                                                       #((top))
                                                       #("i"))
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(x)
                                                       #((top))
                                                       #("i"))
                                                     #(ribcage () () ())
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(x)
                                                       #((top))
                                                       #("i"))
                                                     #(ribcage
                                                       #(quasicons
                                                         quasiappend
                                                         quasivector
                                                         quasi)
                                                       #((top)
                                                         (top)
                                                         (top)
                                                         (top))
                                                       #("i" "i" "i" "i"))))
                                                 syntmp-x-1844))
                                         syntmp-tmp-1843)
                                  ((lambda (syntmp-_-1846)
                                     (list '#(syntax-object
                                              list->vector
                                              ((top)
                                               #(ribcage #(_) #((top)) #("i"))
                                               #(ribcage () () ())
                                               #(ribcage #(x) #((top)) #("i"))
                                               #(ribcage () () ())
                                               #(ribcage () () ())
                                               #(ribcage #(x) #((top)) #("i"))
                                               #(ribcage
                                                 #(quasicons
                                                   quasiappend
                                                   quasivector
                                                   quasi)
                                                 #((top) (top) (top) (top))
                                                 #("i" "i" "i" "i"))))
                                           syntmp-x-1838))
                                   syntmp-tmp-1839)))
                              (syntax-dispatch
                                syntmp-tmp-1839
                                '(#(free-id
                                    #(syntax-object
                                      list
                                      ((top)
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i"))
                                       #(ribcage () () ())
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i"))
                                       #(ribcage
                                         #(quasicons
                                           quasiappend
                                           quasivector
                                           quasi)
                                         #((top) (top) (top) (top))
                                         #("i" "i" "i" "i")))))
                                  .
                                  each-any)))))
                         (syntax-dispatch
                           syntmp-tmp-1839
                           '(#(free-id
                               #(syntax-object
                                 quote
                                 ((top)
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("i"))
                                  #(ribcage () () ())
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("i"))
                                  #(ribcage
                                    #(quasicons quasiappend quasivector quasi)
                                    #((top) (top) (top) (top))
                                    #("i" "i" "i" "i")))))
                             each-any))))
                      syntmp-x-1838))
                   syntmp-tmp-1837))
                syntmp-x-1836)))
           (syntmp-quasi-1810
             (lambda (syntmp-p-1847 syntmp-lev-1848)
               ((lambda (syntmp-tmp-1849)
                  ((lambda (syntmp-tmp-1850)
                     (if syntmp-tmp-1850
                       (apply (lambda (syntmp-p-1851)
                                (if (= syntmp-lev-1848 0)
                                  syntmp-p-1851
                                  (syntmp-quasicons-1807
                                    '(#(syntax-object
                                        quote
                                        ((top)
                                         #(ribcage #(p) #((top)) #("i"))
                                         #(ribcage () () ())
                                         #(ribcage
                                           #(p lev)
                                           #((top) (top))
                                           #("i" "i"))
                                         #(ribcage
                                           #(quasicons
                                             quasiappend
                                             quasivector
                                             quasi)
                                           #((top) (top) (top) (top))
                                           #("i" "i" "i" "i"))))
                                      #(syntax-object
                                        unquote
                                        ((top)
                                         #(ribcage #(p) #((top)) #("i"))
                                         #(ribcage () () ())
                                         #(ribcage
                                           #(p lev)
                                           #((top) (top))
                                           #("i" "i"))
                                         #(ribcage
                                           #(quasicons
                                             quasiappend
                                             quasivector
                                             quasi)
                                           #((top) (top) (top) (top))
                                           #("i" "i" "i" "i")))))
                                    (syntmp-quasi-1810
                                      (list syntmp-p-1851)
                                      (- syntmp-lev-1848 1)))))
                              syntmp-tmp-1850)
                       ((lambda (syntmp-tmp-1852)
                          (if syntmp-tmp-1852
                            (apply (lambda (syntmp-p-1853 syntmp-q-1854)
                                     (if (= syntmp-lev-1848 0)
                                       (syntmp-quasiappend-1808
                                         syntmp-p-1853
                                         (syntmp-quasi-1810
                                           syntmp-q-1854
                                           syntmp-lev-1848))
                                       (syntmp-quasicons-1807
                                         (syntmp-quasicons-1807
                                           '(#(syntax-object
                                               quote
                                               ((top)
                                                #(ribcage
                                                  #(p q)
                                                  #((top) (top))
                                                  #("i" "i"))
                                                #(ribcage () () ())
                                                #(ribcage
                                                  #(p lev)
                                                  #((top) (top))
                                                  #("i" "i"))
                                                #(ribcage
                                                  #(quasicons
                                                    quasiappend
                                                    quasivector
                                                    quasi)
                                                  #((top) (top) (top) (top))
                                                  #("i" "i" "i" "i"))))
                                             #(syntax-object
                                               unquote-splicing
                                               ((top)
                                                #(ribcage
                                                  #(p q)
                                                  #((top) (top))
                                                  #("i" "i"))
                                                #(ribcage () () ())
                                                #(ribcage
                                                  #(p lev)
                                                  #((top) (top))
                                                  #("i" "i"))
                                                #(ribcage
                                                  #(quasicons
                                                    quasiappend
                                                    quasivector
                                                    quasi)
                                                  #((top) (top) (top) (top))
                                                  #("i" "i" "i" "i")))))
                                           (syntmp-quasi-1810
                                             (list syntmp-p-1853)
                                             (- syntmp-lev-1848 1)))
                                         (syntmp-quasi-1810
                                           syntmp-q-1854
                                           syntmp-lev-1848))))
                                   syntmp-tmp-1852)
                            ((lambda (syntmp-tmp-1855)
                               (if syntmp-tmp-1855
                                 (apply (lambda (syntmp-p-1856)
                                          (syntmp-quasicons-1807
                                            '(#(syntax-object
                                                quote
                                                ((top)
                                                 #(ribcage
                                                   #(p)
                                                   #((top))
                                                   #("i"))
                                                 #(ribcage () () ())
                                                 #(ribcage
                                                   #(p lev)
                                                   #((top) (top))
                                                   #("i" "i"))
                                                 #(ribcage
                                                   #(quasicons
                                                     quasiappend
                                                     quasivector
                                                     quasi)
                                                   #((top) (top) (top) (top))
                                                   #("i" "i" "i" "i"))))
                                              #(syntax-object
                                                quasiquote
                                                ((top)
                                                 #(ribcage
                                                   #(p)
                                                   #((top))
                                                   #("i"))
                                                 #(ribcage () () ())
                                                 #(ribcage
                                                   #(p lev)
                                                   #((top) (top))
                                                   #("i" "i"))
                                                 #(ribcage
                                                   #(quasicons
                                                     quasiappend
                                                     quasivector
                                                     quasi)
                                                   #((top) (top) (top) (top))
                                                   #("i" "i" "i" "i")))))
                                            (syntmp-quasi-1810
                                              (list syntmp-p-1856)
                                              (+ syntmp-lev-1848 1))))
                                        syntmp-tmp-1855)
                                 ((lambda (syntmp-tmp-1857)
                                    (if syntmp-tmp-1857
                                      (apply (lambda (syntmp-p-1858
                                                      syntmp-q-1859)
                                               (syntmp-quasicons-1807
                                                 (syntmp-quasi-1810
                                                   syntmp-p-1858
                                                   syntmp-lev-1848)
                                                 (syntmp-quasi-1810
                                                   syntmp-q-1859
                                                   syntmp-lev-1848)))
                                             syntmp-tmp-1857)
                                      ((lambda (syntmp-tmp-1860)
                                         (if syntmp-tmp-1860
                                           (apply (lambda (syntmp-x-1861)
                                                    (syntmp-quasivector-1809
                                                      (syntmp-quasi-1810
                                                        syntmp-x-1861
                                                        syntmp-lev-1848)))
                                                  syntmp-tmp-1860)
                                           ((lambda (syntmp-p-1863)
                                              (list '#(syntax-object
                                                       quote
                                                       ((top)
                                                        #(ribcage
                                                          #(p)
                                                          #((top))
                                                          #("i"))
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(p lev)
                                                          #((top) (top))
                                                          #("i" "i"))
                                                        #(ribcage
                                                          #(quasicons
                                                            quasiappend
                                                            quasivector
                                                            quasi)
                                                          #((top)
                                                            (top)
                                                            (top)
                                                            (top))
                                                          #("i" "i" "i" "i"))))
                                                    syntmp-p-1863))
                                            syntmp-tmp-1849)))
                                       (syntax-dispatch
                                         syntmp-tmp-1849
                                         '#(vector each-any)))))
                                  (syntax-dispatch
                                    syntmp-tmp-1849
                                    '(any . any)))))
                             (syntax-dispatch
                               syntmp-tmp-1849
                               '(#(free-id
                                   #(syntax-object
                                     quasiquote
                                     ((top)
                                      #(ribcage () () ())
                                      #(ribcage
                                        #(p lev)
                                        #((top) (top))
                                        #("i" "i"))
                                      #(ribcage
                                        #(quasicons
                                          quasiappend
                                          quasivector
                                          quasi)
                                        #((top) (top) (top) (top))
                                        #("i" "i" "i" "i")))))
                                 any)))))
                        (syntax-dispatch
                          syntmp-tmp-1849
                          '((#(free-id
                               #(syntax-object
                                 unquote-splicing
                                 ((top)
                                  #(ribcage () () ())
                                  #(ribcage #(p lev) #((top) (top)) #("i" "i"))
                                  #(ribcage
                                    #(quasicons quasiappend quasivector quasi)
                                    #((top) (top) (top) (top))
                                    #("i" "i" "i" "i")))))
                             any)
                            .
                            any)))))
                   (syntax-dispatch
                     syntmp-tmp-1849
                     '(#(free-id
                         #(syntax-object
                           unquote
                           ((top)
                            #(ribcage () () ())
                            #(ribcage #(p lev) #((top) (top)) #("i" "i"))
                            #(ribcage
                              #(quasicons quasiappend quasivector quasi)
                              #((top) (top) (top) (top))
                              #("i" "i" "i" "i")))))
                       any))))
                syntmp-p-1847))))
    (lambda (syntmp-x-1864)
      ((lambda (syntmp-tmp-1865)
         ((lambda (syntmp-tmp-1866)
            (if syntmp-tmp-1866
              (apply (lambda (syntmp-_-1867 syntmp-e-1868)
                       (syntmp-quasi-1810 syntmp-e-1868 0))
                     syntmp-tmp-1866)
              (syntax-error syntmp-tmp-1865)))
          (syntax-dispatch
            syntmp-tmp-1865
            '(any any))))
       syntmp-x-1864))))

(install-global-transformer
  'include
  (lambda (syntmp-x-1928)
    (letrec ((syntmp-read-file-1929
               (lambda (syntmp-fn-1930 syntmp-k-1931)
                 (let ((syntmp-p-1932 (open-input-file syntmp-fn-1930)))
                   (let syntmp-f-1933 ((syntmp-x-1934 (read syntmp-p-1932)))
                     (if (eof-object? syntmp-x-1934)
                       (begin
                         (close-input-port syntmp-p-1932)
                         '())
                       (cons (datum->syntax-object
                               syntmp-k-1931
                               syntmp-x-1934)
                             (syntmp-f-1933 (read syntmp-p-1932)))))))))
      ((lambda (syntmp-tmp-1935)
         ((lambda (syntmp-tmp-1936)
            (if syntmp-tmp-1936
              (apply (lambda (syntmp-k-1937 syntmp-filename-1938)
                       (let ((syntmp-fn-1939
                               (syntax-object->datum syntmp-filename-1938)))
                         ((lambda (syntmp-tmp-1940)
                            ((lambda (syntmp-tmp-1941)
                               (if syntmp-tmp-1941
                                 (apply (lambda (syntmp-exp-1942)
                                          (cons '#(syntax-object
                                                   begin
                                                   ((top)
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(exp)
                                                      #((top))
                                                      #("i"))
                                                    #(ribcage () () ())
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(fn)
                                                      #((top))
                                                      #("i"))
                                                    #(ribcage
                                                      #(k filename)
                                                      #((top) (top))
                                                      #("i" "i"))
                                                    #(ribcage
                                                      (read-file)
                                                      ((top))
                                                      ("i"))
                                                    #(ribcage
                                                      #(x)
                                                      #((top))
                                                      #("i"))))
                                                syntmp-exp-1942))
                                        syntmp-tmp-1941)
                                 (syntax-error syntmp-tmp-1940)))
                             (syntax-dispatch
                               syntmp-tmp-1940
                               'each-any)))
                          (syntmp-read-file-1929
                            syntmp-fn-1939
                            syntmp-k-1937))))
                     syntmp-tmp-1936)
              (syntax-error syntmp-tmp-1935)))
          (syntax-dispatch
            syntmp-tmp-1935
            '(any any))))
       syntmp-x-1928))))

(install-global-transformer
  'unquote
  (lambda (syntmp-x-1959)
    ((lambda (syntmp-tmp-1960)
       ((lambda (syntmp-tmp-1961)
          (if syntmp-tmp-1961
            (apply (lambda (syntmp-_-1962 syntmp-e-1963)
                     (error 'unquote
                            "expression ,~s not valid outside of quasiquote"
                            (syntax-object->datum syntmp-e-1963)))
                   syntmp-tmp-1961)
            (syntax-error syntmp-tmp-1960)))
        (syntax-dispatch
          syntmp-tmp-1960
          '(any any))))
     syntmp-x-1959)))

(install-global-transformer
  'unquote-splicing
  (lambda (syntmp-x-1969)
    ((lambda (syntmp-tmp-1970)
       ((lambda (syntmp-tmp-1971)
          (if syntmp-tmp-1971
            (apply (lambda (syntmp-_-1972 syntmp-e-1973)
                     (error 'unquote-splicing
                            "expression ,@~s not valid outside of quasiquote"
                            (syntax-object->datum syntmp-e-1973)))
                   syntmp-tmp-1971)
            (syntax-error syntmp-tmp-1970)))
        (syntax-dispatch
          syntmp-tmp-1970
          '(any any))))
     syntmp-x-1969)))

(install-global-transformer
  'case
  (lambda (syntmp-x-1979)
    ((lambda (syntmp-tmp-1980)
       ((lambda (syntmp-tmp-1981)
          (if syntmp-tmp-1981
            (apply (lambda (syntmp-_-1982
                            syntmp-e-1983
                            syntmp-m1-1984
                            syntmp-m2-1985)
                     ((lambda (syntmp-tmp-1986)
                        ((lambda (syntmp-body-1987)
                           (list '#(syntax-object
                                    let
                                    ((top)
                                     #(ribcage () () ())
                                     #(ribcage #(body) #((top)) #("i"))
                                     #(ribcage
                                       #(_ e m1 m2)
                                       #((top) (top) (top) (top))
                                       #("i" "i" "i" "i"))
                                     #(ribcage () () ())
                                     #(ribcage #(x) #((top)) #("i"))))
                                 (list (list '#(syntax-object
                                                t
                                                ((top)
                                                 #(ribcage () () ())
                                                 #(ribcage
                                                   #(body)
                                                   #((top))
                                                   #("i"))
                                                 #(ribcage
                                                   #(_ e m1 m2)
                                                   #((top) (top) (top) (top))
                                                   #("i" "i" "i" "i"))
                                                 #(ribcage () () ())
                                                 #(ribcage
                                                   #(x)
                                                   #((top))
                                                   #("i"))))
                                             syntmp-e-1983))
                                 syntmp-body-1987))
                         syntmp-tmp-1986))
                      (let syntmp-f-1988 ((syntmp-clause-1989 syntmp-m1-1984)
                                          (syntmp-clauses-1990 syntmp-m2-1985))
                        (if (null? syntmp-clauses-1990)
                          ((lambda (syntmp-tmp-1992)
                             ((lambda (syntmp-tmp-1993)
                                (if syntmp-tmp-1993
                                  (apply (lambda (syntmp-e1-1994
                                                  syntmp-e2-1995)
                                           (cons '#(syntax-object
                                                    begin
                                                    ((top)
                                                     #(ribcage
                                                       #(e1 e2)
                                                       #((top) (top))
                                                       #("i" "i"))
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(f clause clauses)
                                                       #((top) (top) (top))
                                                       #("i" "i" "i"))
                                                     #(ribcage
                                                       #(_ e m1 m2)
                                                       #((top)
                                                         (top)
                                                         (top)
                                                         (top))
                                                       #("i" "i" "i" "i"))
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(x)
                                                       #((top))
                                                       #("i"))))
                                                 (cons syntmp-e1-1994
                                                       syntmp-e2-1995)))
                                         syntmp-tmp-1993)
                                  ((lambda (syntmp-tmp-1997)
                                     (if syntmp-tmp-1997
                                       (apply (lambda (syntmp-k-1998
                                                       syntmp-e1-1999
                                                       syntmp-e2-2000)
                                                (list '#(syntax-object
                                                         if
                                                         ((top)
                                                          #(ribcage
                                                            #(k e1 e2)
                                                            #((top)
                                                              (top)
                                                              (top))
                                                            #("i" "i" "i"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(f clause clauses)
                                                            #((top)
                                                              (top)
                                                              (top))
                                                            #("i" "i" "i"))
                                                          #(ribcage
                                                            #(_ e m1 m2)
                                                            #((top)
                                                              (top)
                                                              (top)
                                                              (top))
                                                            #("i" "i" "i" "i"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(x)
                                                            #((top))
                                                            #("i"))))
                                                      (list '#(syntax-object
                                                               memv
                                                               ((top)
                                                                #(ribcage
                                                                  #(k e1 e2)
                                                                  #((top)
                                                                    (top)
                                                                    (top))
                                                                  #("i"
                                                                    "i"
                                                                    "i"))
                                                                #(ribcage
                                                                  ()
                                                                  ()
                                                                  ())
                                                                #(ribcage
                                                                  #(f
                                                                    clause
                                                                    clauses)
                                                                  #((top)
                                                                    (top)
                                                                    (top))
                                                                  #("i"
                                                                    "i"
                                                                    "i"))
                                                                #(ribcage
                                                                  #(_ e m1 m2)
                                                                  #((top)
                                                                    (top)
                                                                    (top)
                                                                    (top))
                                                                  #("i"
                                                                    "i"
                                                                    "i"
                                                                    "i"))
                                                                #(ribcage
                                                                  ()
                                                                  ()
                                                                  ())
                                                                #(ribcage
                                                                  #(x)
                                                                  #((top))
                                                                  #("i"))))
                                                            '#(syntax-object
                                                               t
                                                               ((top)
                                                                #(ribcage
                                                                  #(k e1 e2)
                                                                  #((top)
                                                                    (top)
                                                                    (top))
                                                                  #("i"
                                                                    "i"
                                                                    "i"))
                                                                #(ribcage
                                                                  ()
                                                                  ()
                                                                  ())
                                                                #(ribcage
                                                                  #(f
                                                                    clause
                                                                    clauses)
                                                                  #((top)
                                                                    (top)
                                                                    (top))
                                                                  #("i"
                                                                    "i"
                                                                    "i"))
                                                                #(ribcage
                                                                  #(_ e m1 m2)
                                                                  #((top)
                                                                    (top)
                                                                    (top)
                                                                    (top))
                                                                  #("i"
                                                                    "i"
                                                                    "i"
                                                                    "i"))
                                                                #(ribcage
                                                                  ()
                                                                  ()
                                                                  ())
                                                                #(ribcage
                                                                  #(x)
                                                                  #((top))
                                                                  #("i"))))
                                                            (list '#(syntax-object
                                                                     quote
                                                                     ((top)
                                                                      #(ribcage
                                                                        #(k
                                                                          e1
                                                                          e2)
                                                                        #((top)
                                                                          (top)
                                                                          (top))
                                                                        #("i"
                                                                          "i"
                                                                          "i"))
                                                                      #(ribcage
                                                                        ()
                                                                        ()
                                                                        ())
                                                                      #(ribcage
                                                                        #(f
                                                                          clause
                                                                          clauses)
                                                                        #((top)
                                                                          (top)
                                                                          (top))
                                                                        #("i"
                                                                          "i"
                                                                          "i"))
                                                                      #(ribcage
                                                                        #(_
                                                                          e
                                                                          m1
                                                                          m2)
                                                                        #((top)
                                                                          (top)
                                                                          (top)
                                                                          (top))
                                                                        #("i"
                                                                          "i"
                                                                          "i"
                                                                          "i"))
                                                                      #(ribcage
                                                                        ()
                                                                        ()
                                                                        ())
                                                                      #(ribcage
                                                                        #(x)
                                                                        #((top))
                                                                        #("i"))))
                                                                  syntmp-k-1998))
                                                      (cons '#(syntax-object
                                                               begin
                                                               ((top)
                                                                #(ribcage
                                                                  #(k e1 e2)
                                                                  #((top)
                                                                    (top)
                                                                    (top))
                                                                  #("i"
                                                                    "i"
                                                                    "i"))
                                                                #(ribcage
                                                                  ()
                                                                  ()
                                                                  ())
                                                                #(ribcage
                                                                  #(f
                                                                    clause
                                                                    clauses)
                                                                  #((top)
                                                                    (top)
                                                                    (top))
                                                                  #("i"
                                                                    "i"
                                                                    "i"))
                                                                #(ribcage
                                                                  #(_ e m1 m2)
                                                                  #((top)
                                                                    (top)
                                                                    (top)
                                                                    (top))
                                                                  #("i"
                                                                    "i"
                                                                    "i"
                                                                    "i"))
                                                                #(ribcage
                                                                  ()
                                                                  ()
                                                                  ())
                                                                #(ribcage
                                                                  #(x)
                                                                  #((top))
                                                                  #("i"))))
                                                            (cons syntmp-e1-1999
                                                                  syntmp-e2-2000))))
                                              syntmp-tmp-1997)
                                       ((lambda (syntmp-_-2003)
                                          (syntax-error syntmp-x-1979))
                                        syntmp-tmp-1992)))
                                   (syntax-dispatch
                                     syntmp-tmp-1992
                                     '(each-any any . each-any)))))
                              (syntax-dispatch
                                syntmp-tmp-1992
                                '(#(free-id
                                    #(syntax-object
                                      else
                                      ((top)
                                       #(ribcage () () ())
                                       #(ribcage
                                         #(f clause clauses)
                                         #((top) (top) (top))
                                         #("i" "i" "i"))
                                       #(ribcage
                                         #(_ e m1 m2)
                                         #((top) (top) (top) (top))
                                         #("i" "i" "i" "i"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i")))))
                                  any
                                  .
                                  each-any))))
                           syntmp-clause-1989)
                          ((lambda (syntmp-tmp-2004)
                             ((lambda (syntmp-rest-2005)
                                ((lambda (syntmp-tmp-2006)
                                   ((lambda (syntmp-tmp-2007)
                                      (if syntmp-tmp-2007
                                        (apply (lambda (syntmp-k-2008
                                                        syntmp-e1-2009
                                                        syntmp-e2-2010)
                                                 (list '#(syntax-object
                                                          if
                                                          ((top)
                                                           #(ribcage
                                                             #(k e1 e2)
                                                             #((top)
                                                               (top)
                                                               (top))
                                                             #("i" "i" "i"))
                                                           #(ribcage () () ())
                                                           #(ribcage
                                                             #(rest)
                                                             #((top))
                                                             #("i"))
                                                           #(ribcage () () ())
                                                           #(ribcage
                                                             #(f
                                                               clause
                                                               clauses)
                                                             #((top)
                                                               (top)
                                                               (top))
                                                             #("i" "i" "i"))
                                                           #(ribcage
                                                             #(_ e m1 m2)
                                                             #((top)
                                                               (top)
                                                               (top)
                                                               (top))
                                                             #("i"
                                                               "i"
                                                               "i"
                                                               "i"))
                                                           #(ribcage () () ())
                                                           #(ribcage
                                                             #(x)
                                                             #((top))
                                                             #("i"))))
                                                       (list '#(syntax-object
                                                                memv
                                                                ((top)
                                                                 #(ribcage
                                                                   #(k e1 e2)
                                                                   #((top)
                                                                     (top)
                                                                     (top))
                                                                   #("i"
                                                                     "i"
                                                                     "i"))
                                                                 #(ribcage
                                                                   ()
                                                                   ()
                                                                   ())
                                                                 #(ribcage
                                                                   #(rest)
                                                                   #((top))
                                                                   #("i"))
                                                                 #(ribcage
                                                                   ()
                                                                   ()
                                                                   ())
                                                                 #(ribcage
                                                                   #(f
                                                                     clause
                                                                     clauses)
                                                                   #((top)
                                                                     (top)
                                                                     (top))
                                                                   #("i"
                                                                     "i"
                                                                     "i"))
                                                                 #(ribcage
                                                                   #(_ e m1 m2)
                                                                   #((top)
                                                                     (top)
                                                                     (top)
                                                                     (top))
                                                                   #("i"
                                                                     "i"
                                                                     "i"
                                                                     "i"))
                                                                 #(ribcage
                                                                   ()
                                                                   ()
                                                                   ())
                                                                 #(ribcage
                                                                   #(x)
                                                                   #((top))
                                                                   #("i"))))
                                                             '#(syntax-object
                                                                t
                                                                ((top)
                                                                 #(ribcage
                                                                   #(k e1 e2)
                                                                   #((top)
                                                                     (top)
                                                                     (top))
                                                                   #("i"
                                                                     "i"
                                                                     "i"))
                                                                 #(ribcage
                                                                   ()
                                                                   ()
                                                                   ())
                                                                 #(ribcage
                                                                   #(rest)
                                                                   #((top))
                                                                   #("i"))
                                                                 #(ribcage
                                                                   ()
                                                                   ()
                                                                   ())
                                                                 #(ribcage
                                                                   #(f
                                                                     clause
                                                                     clauses)
                                                                   #((top)
                                                                     (top)
                                                                     (top))
                                                                   #("i"
                                                                     "i"
                                                                     "i"))
                                                                 #(ribcage
                                                                   #(_ e m1 m2)
                                                                   #((top)
                                                                     (top)
                                                                     (top)
                                                                     (top))
                                                                   #("i"
                                                                     "i"
                                                                     "i"
                                                                     "i"))
                                                                 #(ribcage
                                                                   ()
                                                                   ()
                                                                   ())
                                                                 #(ribcage
                                                                   #(x)
                                                                   #((top))
                                                                   #("i"))))
                                                             (list '#(syntax-object
                                                                      quote
                                                                      ((top)
                                                                       #(ribcage
                                                                         #(k
                                                                           e1
                                                                           e2)
                                                                         #((top)
                                                                           (top)
                                                                           (top))
                                                                         #("i"
                                                                           "i"
                                                                           "i"))
                                                                       #(ribcage
                                                                         ()
                                                                         ()
                                                                         ())
                                                                       #(ribcage
                                                                         #(rest)
                                                                         #((top))
                                                                         #("i"))
                                                                       #(ribcage
                                                                         ()
                                                                         ()
                                                                         ())
                                                                       #(ribcage
                                                                         #(f
                                                                           clause
                                                                           clauses)
                                                                         #((top)
                                                                           (top)
                                                                           (top))
                                                                         #("i"
                                                                           "i"
                                                                           "i"))
                                                                       #(ribcage
                                                                         #(_
                                                                           e
                                                                           m1
                                                                           m2)
                                                                         #((top)
                                                                           (top)
                                                                           (top)
                                                                           (top))
                                                                         #("i"
                                                                           "i"
                                                                           "i"
                                                                           "i"))
                                                                       #(ribcage
                                                                         ()
                                                                         ()
                                                                         ())
                                                                       #(ribcage
                                                                         #(x)
                                                                         #((top))
                                                                         #("i"))))
                                                                   syntmp-k-2008))
                                                       (cons '#(syntax-object
                                                                begin
                                                                ((top)
                                                                 #(ribcage
                                                                   #(k e1 e2)
                                                                   #((top)
                                                                     (top)
                                                                     (top))
                                                                   #("i"
                                                                     "i"
                                                                     "i"))
                                                                 #(ribcage
                                                                   ()
                                                                   ()
                                                                   ())
                                                                 #(ribcage
                                                                   #(rest)
                                                                   #((top))
                                                                   #("i"))
                                                                 #(ribcage
                                                                   ()
                                                                   ()
                                                                   ())
                                                                 #(ribcage
                                                                   #(f
                                                                     clause
                                                                     clauses)
                                                                   #((top)
                                                                     (top)
                                                                     (top))
                                                                   #("i"
                                                                     "i"
                                                                     "i"))
                                                                 #(ribcage
                                                                   #(_ e m1 m2)
                                                                   #((top)
                                                                     (top)
                                                                     (top)
                                                                     (top))
                                                                   #("i"
                                                                     "i"
                                                                     "i"
                                                                     "i"))
                                                                 #(ribcage
                                                                   ()
                                                                   ()
                                                                   ())
                                                                 #(ribcage
                                                                   #(x)
                                                                   #((top))
                                                                   #("i"))))
                                                             (cons syntmp-e1-2009
                                                                   syntmp-e2-2010))
                                                       syntmp-rest-2005))
                                               syntmp-tmp-2007)
                                        ((lambda (syntmp-_-2013)
                                           (syntax-error syntmp-x-1979))
                                         syntmp-tmp-2006)))
                                    (syntax-dispatch
                                      syntmp-tmp-2006
                                      '(each-any any . each-any))))
                                 syntmp-clause-1989))
                              syntmp-tmp-2004))
                           (syntmp-f-1988
                             (car syntmp-clauses-1990)
                             (cdr syntmp-clauses-1990)))))))
                   syntmp-tmp-1981)
            (syntax-error syntmp-tmp-1980)))
        (syntax-dispatch
          syntmp-tmp-1980
          '(any any any . each-any))))
     syntmp-x-1979)))

(install-global-transformer
  'identifier-syntax
  (lambda (syntmp-x-2043)
    ((lambda (syntmp-tmp-2044)
       ((lambda (syntmp-tmp-2045)
          (if syntmp-tmp-2045
            (apply (lambda (syntmp-_-2046 syntmp-e-2047)
                     (list '#(syntax-object
                              lambda
                              ((top)
                               #(ribcage #(_ e) #((top) (top)) #("i" "i"))
                               #(ribcage () () ())
                               #(ribcage #(x) #((top)) #("i"))))
                           '(#(syntax-object
                               x
                               ((top)
                                #(ribcage #(_ e) #((top) (top)) #("i" "i"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i")))))
                           (list '#(syntax-object
                                    syntax-case
                                    ((top)
                                     #(ribcage
                                       #(_ e)
                                       #((top) (top))
                                       #("i" "i"))
                                     #(ribcage () () ())
                                     #(ribcage #(x) #((top)) #("i"))))
                                 '#(syntax-object
                                    x
                                    ((top)
                                     #(ribcage
                                       #(_ e)
                                       #((top) (top))
                                       #("i" "i"))
                                     #(ribcage () () ())
                                     #(ribcage #(x) #((top)) #("i"))))
                                 '()
                                 (list '#(syntax-object
                                          id
                                          ((top)
                                           #(ribcage
                                             #(_ e)
                                             #((top) (top))
                                             #("i" "i"))
                                           #(ribcage () () ())
                                           #(ribcage #(x) #((top)) #("i"))))
                                       '(#(syntax-object
                                           identifier?
                                           ((top)
                                            #(ribcage
                                              #(_ e)
                                              #((top) (top))
                                              #("i" "i"))
                                            #(ribcage () () ())
                                            #(ribcage #(x) #((top)) #("i"))))
                                         (#(syntax-object
                                            syntax
                                            ((top)
                                             #(ribcage
                                               #(_ e)
                                               #((top) (top))
                                               #("i" "i"))
                                             #(ribcage () () ())
                                             #(ribcage #(x) #((top)) #("i"))))
                                          #(syntax-object
                                            id
                                            ((top)
                                             #(ribcage
                                               #(_ e)
                                               #((top) (top))
                                               #("i" "i"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x)
                                               #((top))
                                               #("i"))))))
                                       (list '#(syntax-object
                                                syntax
                                                ((top)
                                                 #(ribcage
                                                   #(_ e)
                                                   #((top) (top))
                                                   #("i" "i"))
                                                 #(ribcage () () ())
                                                 #(ribcage
                                                   #(x)
                                                   #((top))
                                                   #("i"))))
                                             syntmp-e-2047))
                                 (list (cons syntmp-_-2046
                                             '(#(syntax-object
                                                 x
                                                 ((top)
                                                  #(ribcage
                                                    #(_ e)
                                                    #((top) (top))
                                                    #("i" "i"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(x)
                                                    #((top))
                                                    #("i"))))
                                               #(syntax-object
                                                 ...
                                                 ((top)
                                                  #(ribcage
                                                    #(_ e)
                                                    #((top) (top))
                                                    #("i" "i"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(x)
                                                    #((top))
                                                    #("i"))))))
                                       (list '#(syntax-object
                                                syntax
                                                ((top)
                                                 #(ribcage
                                                   #(_ e)
                                                   #((top) (top))
                                                   #("i" "i"))
                                                 #(ribcage () () ())
                                                 #(ribcage
                                                   #(x)
                                                   #((top))
                                                   #("i"))))
                                             (cons syntmp-e-2047
                                                   '(#(syntax-object
                                                       x
                                                       ((top)
                                                        #(ribcage
                                                          #(_ e)
                                                          #((top) (top))
                                                          #("i" "i"))
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(x)
                                                          #((top))
                                                          #("i"))))
                                                     #(syntax-object
                                                       ...
                                                       ((top)
                                                        #(ribcage
                                                          #(_ e)
                                                          #((top) (top))
                                                          #("i" "i"))
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(x)
                                                          #((top))
                                                          #("i")))))))))))
                   syntmp-tmp-2045)
            (syntax-error syntmp-tmp-2044)))
        (syntax-dispatch
          syntmp-tmp-2044
          '(any any))))
     syntmp-x-2043)))

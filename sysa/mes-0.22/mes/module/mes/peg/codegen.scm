;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright (C) 2011 Free Software Foundation, Inc.
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

;;; Taken from GNU Guile

;;; codegen.scm --- code generation for composable parsers

(define-module (ice-9 peg codegen)
  #:export (compile-peg-pattern wrap-parser-for-users add-peg-compiler!)
  #:use-module (ice-9 pretty-print)
  #:use-module (system base pmatch))

(define-syntax single?
  (syntax-rules ()
    ;;"Return #t if X is a list of one element."
    ((_ x)
     (pmatch x
       ((_) #t)
       (else #f)))))

(define-syntax single-filter
  (syntax-rules ()
    ;;"If EXP is a list of one element, return the element.  Otherwise return EXP."
    ((_ exp)
     (pmatch exp
       ((,elt) elt)
       (,elts elts)))))

(define-syntax push-not-null!
  (syntax-rules ()
    ;;"If OBJ is non-null, push it onto LST, otherwise do nothing."
    ((_ lst obj)
     (if (not (null? obj))
         (push! lst obj)))))

(define-syntax push!
  (syntax-rules ()
    ;;"Push an object onto a list."
    ((_ lst obj)
     (set! lst (cons obj lst)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; CODE GENERATORS
;; These functions generate scheme code for parsing PEGs.
;; Conventions:
;;   accum: (all name body none)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Code we generate will have a certain return structure depending on how we're
;; accumulating (the ACCUM variable).
(define (cg-generic-ret accum name body-uneval at)
  ;; name, body-uneval and at are syntax
  #`(let ((body #,body-uneval))
     #,(cond
        ((and (eq? accum 'all) name)
         #`(list #,at
                 (cond
                  ((not (list? body)) (list '#,name body))
                  ((null? body) '#,name)
                  ((symbol? (car body)) (list '#,name body))
                  (else (cons '#,name body)))))
        ((eq? accum 'name)
         #`(list #,at '#,name))
        ((eq? accum 'body)
         #`(list #,at
                 (cond
                  ((single? body) (car body))
                  (else body))))
        ((eq? accum 'none)
         #`(list #,at '()))
        (else
         (begin
           (pretty-print `(cg-generic-ret-error ,accum ,name ,body-uneval ,at))
           (pretty-print "Defaulting to accum of none.\n")
           #`(list #,at '()))))))

;; The short name makes the formatting below much easier to read.
(define cggr cg-generic-ret)

;; Generates code that matches a particular string.
;; E.g.: (cg-string syntax "abc" 'body)
(define (cg-string pat accum)
  (let ((plen (string-length pat)))
    #`(lambda (str len pos)
        (let ((end (+ pos #,plen)))
          (and (<= end len)
               (string= str #,pat pos end)
               #,(case accum
                   ((all) #`(list end (list 'cg-string #,pat)))
                   ((name) #`(list end 'cg-string))
                   ((body) #`(list end #,pat))
                   ((none) #`(list end '()))
                   (else (error "bad accum" accum))))))))

;; Generates code for matching any character.
;; E.g.: (cg-peg-any syntax 'body)
(define (cg-peg-any accum)
  #`(lambda (str len pos)
      (and (< pos len)
           #,(case accum
               ((all) #`(list (1+ pos)
                              (list 'cg-peg-any (substring str pos (1+ pos)))))
               ((name) #`(list (1+ pos) 'cg-peg-any))
               ((body) #`(list (1+ pos) (substring str pos (1+ pos))))
               ((none) #`(list (1+ pos) '()))
               (else (error "bad accum" accum))))))

;; Generates code for matching a range of characters between start and end.
;; E.g.: (cg-range syntax #\a #\z 'body)
(define (cg-range pat accum)
  (syntax-case pat ()
    ((start end)
     (if (not (and (char? (syntax->datum #'start))
                   (char? (syntax->datum #'end))))
         (error "range PEG should have characters after it; instead got"
                #'start #'end))
     #`(lambda (str len pos)
         (and (< pos len)
              (let ((c (string-ref str pos)))
                (and (char>=? c start)
                     (char<=? c end)
                     #,(case accum
                         ((all) #`(list (1+ pos) (list 'cg-range (string c))))
                         ((name) #`(list (1+ pos) 'cg-range))
                         ((body) #`(list (1+ pos) (string c)))
                         ((none) #`(list (1+ pos) '()))
                         (else (error "bad accum" accum))))))))))

;; Generate code to match a pattern and do nothing with the result
(define (cg-ignore pat accum)
  (syntax-case pat ()
    ((inner)
     (compile-peg-pattern #'inner 'none))))

(define (cg-capture pat accum)
  (syntax-case pat ()
    ((inner)
     (compile-peg-pattern #'inner 'body))))

;; Filters the accum argument to compile-peg-pattern for buildings like string
;; literals (since we don't want to tag them with their name if we're doing an
;; "all" accum).
(define (builtin-accum-filter accum)
  (cond
   ((eq? accum 'all) 'body)
   ((eq? accum 'name) 'name)
   ((eq? accum 'body) 'body)
   ((eq? accum 'none) 'none)))
(define baf builtin-accum-filter)

;; Top-level function builder for AND.  Reduces to a call to CG-AND-INT.
(define (cg-and clauses accum)
  #`(lambda (str len pos)
      (let ((body '()))
        #,(cg-and-int clauses (baf accum) #'str #'len #'pos #'body))))

;; Internal function builder for AND (calls itself).
(define (cg-and-int clauses accum str strlen at body)
  (syntax-case clauses ()
    (()
     (cggr accum 'cg-and #`(reverse #,body) at))
    ((first rest ...)
     #`(let ((res (#,(compile-peg-pattern #'first accum) #,str #,strlen #,at)))
         (and res
              ;; update AT and BODY then recurse
              (let ((newat (car res))
                    (newbody (cadr res)))
                (set! #,at newat)
                (push-not-null! #,body (single-filter newbody))
                #,(cg-and-int #'(rest ...) accum str strlen at body)))))))

;; Top-level function builder for OR.  Reduces to a call to CG-OR-INT.
(define (cg-or clauses accum)
  #`(lambda (str len pos)
      #,(cg-or-int clauses (baf accum) #'str #'len #'pos)))

;; Internal function builder for OR (calls itself).
(define (cg-or-int clauses accum str strlen at)
  (syntax-case clauses ()
    (()
     #f)
    ((first rest ...)
     #`(or (#,(compile-peg-pattern #'first accum) #,str #,strlen #,at)
           #,(cg-or-int #'(rest ...) accum str strlen at)))))

(define (cg-* args accum)
  (syntax-case args ()
    ((pat)
     #`(lambda (str strlen at)
         (let ((body '()))
           (let lp ((end at) (count 0))
             (let* ((match (#,(compile-peg-pattern #'pat (baf accum))
                            str strlen end))
                    (new-end (if match (car match) end))
                    (count (if (> new-end end) (1+ count) count)))
               (if (> new-end end)
                   (push-not-null! body (single-filter (cadr match))))
               (if (and (> new-end end)
                        #,#t)
                   (lp new-end count)
                   (let ((success #,#t))
                     #,#`(and success
                                 #,(cggr (baf accum) 'cg-body
                                         #'(reverse body) #'new-end)))))))))))

(define (cg-+ args accum)
  (syntax-case args ()
    ((pat)
     #`(lambda (str strlen at)
         (let ((body '()))
           (let lp ((end at) (count 0))
             (let* ((match (#,(compile-peg-pattern #'pat (baf accum))
                            str strlen end))
                    (new-end (if match (car match) end))
                    (count (if (> new-end end) (1+ count) count)))
               (if (> new-end end)
                   (push-not-null! body (single-filter (cadr match))))
               (if (and (> new-end end)
                        #,#t)
                   (lp new-end count)
                   (let ((success #,#'(>= count 1)))
                     #,#`(and success
                                 #,(cggr (baf accum) 'cg-body
                                         #'(reverse body) #'new-end)))))))))))

(define (cg-? args accum)
  (syntax-case args ()
    ((pat)
     #`(lambda (str strlen at)
         (let ((body '()))
           (let lp ((end at) (count 0))
             (let* ((match (#,(compile-peg-pattern #'pat (baf accum))
                            str strlen end))
                    (new-end (if match (car match) end))
                    (count (if (> new-end end) (1+ count) count)))
               (if (> new-end end)
                   (push-not-null! body (single-filter (cadr match))))
               (if (and (> new-end end)
                        #,#'(< count 1))
                   (lp new-end count)
                   (let ((success #,#t))
                     #,#`(and success
                                 #,(cggr (baf accum) 'cg-body
                                         #'(reverse body) #'new-end)))))))))))

(define (cg-followed-by args accum)
  (syntax-case args ()
    ((pat)
     #`(lambda (str strlen at)
         (let ((body '()))
           (let lp ((end at) (count 0))
             (let* ((match (#,(compile-peg-pattern #'pat (baf accum))
                            str strlen end))
                    (new-end (if match (car match) end))
                    (count (if (> new-end end) (1+ count) count)))
               (if (> new-end end)
                   (push-not-null! body (single-filter (cadr match))))
               (if (and (> new-end end)
                        #,#'(< count 1))
                   (lp new-end count)
                   (let ((success #,#'(= count 1)))
                     #,#`(and success
                              #,(cggr (baf accum) 'cg-body #''() #'at)))))))))))

(define (cg-not-followed-by args accum)
  (syntax-case args ()
    ((pat)
     #`(lambda (str strlen at)
         (let ((body '()))
           (let lp ((end at) (count 0))
             (let* ((match (#,(compile-peg-pattern #'pat (baf accum))
                            str strlen end))
                    (new-end (if match (car match) end))
                    (count (if (> new-end end) (1+ count) count)))
               (if (> new-end end)
                   (push-not-null! body (single-filter (cadr match))))
               (if (and (> new-end end)
                        #,#'(< count 1))
                   (lp new-end count)
                   (let ((success #,#'(= count 1)))
                     #,#`(if success
                                #f
                                #,(cggr (baf accum) 'cg-body #''() #'at)))))))))))

;; Association list of functions to handle different expressions as PEGs
(define peg-compiler-alist '())

(define (add-peg-compiler! symbol function)
  (set! peg-compiler-alist
        (assq-set! peg-compiler-alist symbol function)))

(add-peg-compiler! 'range cg-range)
(add-peg-compiler! 'ignore cg-ignore)
(add-peg-compiler! 'capture cg-capture)
(add-peg-compiler! 'and cg-and)
(add-peg-compiler! 'or cg-or)
(add-peg-compiler! '* cg-*)
(add-peg-compiler! '+ cg-+)
(add-peg-compiler! '? cg-?)
(add-peg-compiler! 'followed-by cg-followed-by)
(add-peg-compiler! 'not-followed-by cg-not-followed-by)

;; Takes an arbitrary expressions and accumulation variable, then parses it.
;; E.g.: (compile-peg-pattern syntax '(and "abc" (or "-" (range #\a #\z))) 'all)
(define (compile-peg-pattern pat accum)
  (syntax-case pat (peg-any)
    (peg-any
     (cg-peg-any (baf accum)))
    (sym (identifier? #'sym) ;; nonterminal
     #'sym)
    (str (string? (syntax->datum #'str)) ;; literal string
     (cg-string (syntax->datum #'str) (baf accum)))
    ((name . args) (let* ((nm (syntax->datum #'name))
                          (entry (assq-ref peg-compiler-alist nm)))
                     (if entry
                         (entry #'args accum)
                         (error "Bad peg form" nm #'args
                                "Not one of" (map car peg-compiler-alist)))))))

;; Packages the results of a parser
(define (wrap-parser-for-users for-syntax parser accumsym s-syn)
   #`(lambda (str strlen at)
      (let ((res (#,parser str strlen at)))
        ;; Try to match the nonterminal.
        (if res
            ;; If we matched, do some post-processing to figure out
            ;; what data to propagate upward.
            (let ((at (car res))
                  (body (cadr res)))
              #,(cond
                 ((eq? accumsym 'name)
                  #`(list at '#,s-syn))
                 ((eq? accumsym 'all)
                  #`(list (car res)
                          (cond
                           ((not (list? body))
                            (list '#,s-syn body))
                           ((null? body) '#,s-syn)
                           ((symbol? (car body))
                            (list '#,s-syn body))
                           (else (cons '#,s-syn body)))))
                 ((eq? accumsym 'none) #`(list (car res) '()))
                 (else #`(begin res))))
            ;; If we didn't match, just return false.
            #f))))

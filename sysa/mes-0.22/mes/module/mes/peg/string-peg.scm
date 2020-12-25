;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright (C) 2010, 2011 Free Software Foundation, Inc.
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

;;; string-peg.scm --- representing PEG grammars as strings

(define-module (ice-9 peg string-peg)
  #:export (peg-as-peg
            define-peg-string-patterns
            peg-grammar)
  #:use-module (ice-9 peg using-parsers)
  #:use-module (ice-9 peg codegen)
  #:use-module (ice-9 peg simplify-tree))

;; Gets the left-hand depth of a list.
(define (depth lst)
  (if (or (not (list? lst)) (null? lst))
      0
      (+ 1 (depth (car lst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Parse string PEGs using sexp PEGs.
;; See the variable PEG-AS-PEG for an easier-to-read syntax.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Grammar for PEGs in PEG grammar.
(define peg-as-peg
"grammar <-- (nonterminal ('<--' / '<-' / '<') sp pattern)+
pattern <-- alternative (SLASH sp alternative)*
alternative <-- ([!&]? sp suffix)+
suffix <-- primary ([*+?] sp)*
primary <-- '(' sp pattern ')' sp / '.' sp / literal / charclass / nonterminal !'<'
literal <-- ['] (!['] .)* ['] sp
charclass <-- LB (!']' (CCrange / CCsingle))* RB sp
CCrange <-- . '-' .
CCsingle <-- .
nonterminal <-- [a-zA-Z0-9-]+ sp
sp < [ \t\n]*
SLASH < '/'
LB < '['
RB < ']'
")

(define-syntax define-sexp-parser
  (lambda (x)
    (syntax-case x ()
      ((_ sym accum pat)
       (let* ((matchf (compile-peg-pattern #'pat (syntax->datum #'accum)))
              (accumsym (syntax->datum #'accum))
              (syn (wrap-parser-for-users x matchf accumsym #'sym)))
           #`(define sym #,syn))))))

(define-sexp-parser peg-grammar all
  (+ (and peg-nonterminal (or "<--" "<-" "<") peg-sp peg-pattern)))
(define-sexp-parser peg-pattern all
  (and peg-alternative
       (* (and (ignore "/") peg-sp peg-alternative))))
(define-sexp-parser peg-alternative all
  (+ (and (? (or "!" "&")) peg-sp peg-suffix)))
(define-sexp-parser peg-suffix all
  (and peg-primary (* (and (or "*" "+" "?") peg-sp))))
(define-sexp-parser peg-primary all
  (or (and "(" peg-sp peg-pattern ")" peg-sp)
      (and "." peg-sp)
      peg-literal
      peg-charclass
      (and peg-nonterminal (not-followed-by "<"))))
(define-sexp-parser peg-literal all
  (and "'" (* (and (not-followed-by "'") peg-any)) "'" peg-sp))
(define-sexp-parser peg-charclass all
  (and (ignore "[")
       (* (and (not-followed-by "]")
               (or charclass-range charclass-single)))
       (ignore "]")
       peg-sp))
(define-sexp-parser charclass-range all (and peg-any "-" peg-any))
(define-sexp-parser charclass-single all peg-any)
(define-sexp-parser peg-nonterminal all
  (and (+ (or (range #\a #\z) (range #\A #\Z) (range #\0 #\9) "-")) peg-sp))
(define-sexp-parser peg-sp none
  (* (or " " "\t" "\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; PARSE STRING PEGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Takes a string representing a PEG grammar and returns syntax that
;; will define all of the nonterminals in the grammar with equivalent
;; PEG s-expressions.
(define (peg-parser str for-syntax)
  (let ((parsed (match-pattern peg-grammar str)))
    (if (not parsed)
        (begin
          ;; (display "Invalid PEG grammar!\n")
          #f)
        (let ((lst (peg:tree parsed)))
          (cond
           ((or (not (list? lst)) (null? lst))
            lst)
           ((eq? (car lst) 'peg-grammar)
            #`(begin
                #,@(map (lambda (x) (peg-nonterm->defn x for-syntax))
                        (context-flatten (lambda (lst) (<= (depth lst) 2))
                                         (cdr lst))))))))))

;; Macro wrapper for PEG-PARSER.  Parses PEG grammars expressed as strings and
;; defines all the appropriate nonterminals.
(define-syntax define-peg-string-patterns
  (lambda (x)
    (syntax-case x ()
      ((_ str)
       (peg-parser (syntax->datum #'str) x)))))

;; lst has format (nonterm grabber pattern), where
;;   nonterm is a symbol (the name of the nonterminal),
;;   grabber is a string (either "<", "<-" or "<--"), and
;;   pattern is the parse of a PEG pattern expressed as as string.
(define (peg-nonterm->defn lst for-syntax)
  (let* ((nonterm (car lst))
         (grabber (cadr lst))
         (pattern (caddr lst))
         (nonterm-name (datum->syntax for-syntax
                                      (string->symbol (cadr nonterm)))))
    #`(define-peg-pattern #,nonterm-name
       #,(cond
          ((string=? grabber "<--") (datum->syntax for-syntax 'all))
          ((string=? grabber "<-") (datum->syntax for-syntax 'body))
          (else (datum->syntax for-syntax 'none)))
       #,(compressor (peg-pattern->defn pattern for-syntax) for-syntax))))

;; lst has format ('peg-pattern ...).
;; After the context-flatten, (cdr lst) has format
;;   (('peg-alternative ...) ...), where the outer list is a collection
;;   of elements from a '/' alternative.
(define (peg-pattern->defn lst for-syntax)
  #`(or #,@(map (lambda (x) (peg-alternative->defn x for-syntax))
                (context-flatten (lambda (x) (eq? (car x) 'peg-alternative))
                                 (cdr lst)))))

;; lst has format ('peg-alternative ...).
;; After the context-flatten, (cdr lst) has the format
;;   (item ...), where each item has format either ("!" ...), ("&" ...),
;;   or ('peg-suffix ...).
(define (peg-alternative->defn lst for-syntax)
  #`(and #,@(map (lambda (x) (peg-body->defn x for-syntax))
                 (context-flatten (lambda (x) (or (string? (car x))
                                             (eq? (car x) 'peg-suffix)))
                                  (cdr lst)))))

;; lst has the format either
;;   ("!" ('peg-suffix ...)), ("&" ('peg-suffix ...)), or
;;     ('peg-suffix ...).
(define (peg-body->defn lst for-syntax)
    (cond
      ((equal? (car lst) "&")
       #`(followed-by #,(peg-suffix->defn (cadr lst) for-syntax)))
      ((equal? (car lst) "!")
       #`(not-followed-by #,(peg-suffix->defn (cadr lst) for-syntax)))
      ((eq? (car lst) 'peg-suffix)
       (peg-suffix->defn lst for-syntax))
      (else `(peg-parse-body-fail ,lst))))

;; lst has format ('peg-suffix <peg-primary> (? (/ "*" "?" "+")))
(define (peg-suffix->defn lst for-syntax)
  (let ((inner-defn (peg-primary->defn (cadr lst) for-syntax)))
    (cond
      ((null? (cddr lst))
       inner-defn)
      ((equal? (caddr lst) "*")
       #`(* #,inner-defn))
      ((equal? (caddr lst) "?")
       #`(? #,inner-defn))
      ((equal? (caddr lst) "+")
       #`(+ #,inner-defn)))))

;; Parse a primary.
(define (peg-primary->defn lst for-syntax)
  (let ((el (cadr lst)))
  (cond
   ((list? el)
    (cond
     ((eq? (car el) 'peg-literal)
      (peg-literal->defn el for-syntax))
     ((eq? (car el) 'peg-charclass)
      (peg-charclass->defn el for-syntax))
     ((eq? (car el) 'peg-nonterminal)
      (datum->syntax for-syntax (string->symbol (cadr el))))))
   ((string? el)
    (cond
     ((equal? el "(")
      (peg-pattern->defn (caddr lst) for-syntax))
     ((equal? el ".")
      (datum->syntax for-syntax 'peg-any))
     (else (datum->syntax for-syntax
                          `(peg-parse-any unknown-string ,lst)))))
   (else (datum->syntax for-syntax
                        `(peg-parse-any unknown-el ,lst))))))

;; Trims characters off the front and end of STR.
;; (trim-1chars "'ab'") -> "ab"
(define (trim-1chars str) (substring str 1 (- (string-length str) 1)))

;; Parses a literal.
(define (peg-literal->defn lst for-syntax)
  (datum->syntax for-syntax (trim-1chars (cadr lst))))

;; Parses a charclass.
(define (peg-charclass->defn lst for-syntax)
  #`(or
     #,@(map
         (lambda (cc)
           (cond
            ((eq? (car cc) 'charclass-range)
             #`(range #,(datum->syntax
                         for-syntax
                         (string-ref (cadr cc) 0))
                      #,(datum->syntax
                         for-syntax
                         (string-ref (cadr cc) 2))))
            ((eq? (car cc) 'charclass-single)
             (datum->syntax for-syntax (cadr cc)))))
         (context-flatten
          (lambda (x) (or (eq? (car x) 'charclass-range)
                          (eq? (car x) 'charclass-single)))
          (cdr lst)))))

;; Compresses a list to save the optimizer work.
;; e.g. (or (and a)) -> a
(define (compressor-core lst)
  (if (or (not (list? lst)) (null? lst))
      lst
      (cond
       ((and (or (eq? (car lst) 'or) (eq? (car lst) 'and))
             (null? (cddr lst)))
        (compressor-core (cadr lst)))
       ((and (eq? (car lst) 'body)
             (eq? (cadr lst) 'lit)
             (eq? (cadddr lst) 1))
        (compressor-core (caddr lst)))
       (else (map compressor-core lst)))))

(define (compressor syn for-syntax)
  (datum->syntax for-syntax
                 (compressor-core (syntax->datum syn))))

;; Builds a lambda-expressions for the pattern STR using accum.
(define (peg-string-compile args accum)
  (syntax-case args ()
    ((str-stx) (string? (syntax->datum #'str-stx))
     (let ((string (syntax->datum #'str-stx)))
       (compile-peg-pattern
        (compressor
         (peg-pattern->defn
          (peg:tree (match-pattern peg-pattern string)) #'str-stx)
         #'str-stx)
        (if (eq? accum 'all) 'body accum))))
     (else (error "Bad embedded PEG string" args))))

(add-peg-compiler! 'peg peg-string-compile)

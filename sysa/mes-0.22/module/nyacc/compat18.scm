;;; nyacc/compat18.scm - V18 compatibility, used by some for debugging

;; Copyright (C) 2017 Matthew R. Wette
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this library; if not, see <http://www.gnu.org/licenses/>

;;; Code:

(define-module (nyacc compat18)
  #:export (vector-map
	    vector-for-each vector-any vector-fold
	    syntax->datum datum->syntax
	    bitwise-arithmetic-shift-left
	    bitwise-arithmetic-shift-right
	    )
  #:export-syntax (unless when pmatch include-from-path)
  #:use-syntax (ice-9 syncase))

;; replacement for same from (srfi srfi-43)
(define (vector-map proc . vecs)
  (let* ((size (apply min (map vector-length vecs)))
	 (retv (make-vector size)))
    (let loop ((ix 0))
      (cond
       ((= ix size) retv)
       (else
	(vector-set! retv ix
		     (apply proc ix (map (lambda (v) (vector-ref v ix)) vecs)))
	(loop (1+ ix)))))))

;; replacement for same from (srfi srfi-43)
(define (vector-for-each proc . vecs)
  (let ((size (apply min (map vector-length vecs))))
    (let loop ((ix 0))
      (cond
       ((= ix size) (if #f #f))
       (else
	(apply proc ix (map (lambda (v) (vector-ref v ix)) vecs))
	(loop (1+ ix)))))))
  
;; hack to replace same from (srfi srfi-43)
;; the real one takes more args
(define (vector-any pred? vec)
  (let ((size (vector-length vec)))
    (let loop ((ix 0))
      (cond
       ((= ix size) #f)
       ((pred? ix (vector-ref vec ix)) #t)
       (else (loop (1+ ix)))))))

;; replacement for same from (srfi srfi-43)
(define (vector-fold proc seed . vecs)
  (let ((size (apply min (map vector-length vecs))))
    (let loop ((seed seed) (ix 0))
      (cond
       ((= ix size) seed)
       (else
	(loop
	 (apply proc ix seed (map (lambda (v) (vector-ref v ix)) vecs))
	 (1+ ix)))))))

;; change in syntax-case names
(define datum->syntax datum->syntax-object)
(define syntax->datum syntax-object->datum)

(define-syntax unless
  (syntax-rules ()
    ((_ c e ...) (if (not c) (begin e ...)))))

(define-syntax when
  (syntax-rules ()
    ((_ c e ...) (if c (begin e ...)))))

(define (bitwise-arithmetic-shift-right ei1 ei2)
  (let loop ((ei1 ei1) (ei2 ei2))
    (if (zero? ei2) ei1
	(loop (quotient ei2 2) (1- ei1)))))

(define (bitwise-arithmetic-shift-left ei1 ei2)
  (let loop ((ei1 ei1) (ei2 ei2))
    (if (zero? ei2) ei1
	(loop (* ei2 2) (1- ei1)))))

(define-syntax pmatch
  (syntax-rules ()
    ((_ e cs ...)
     (let ((v e)) (pmatch1 v cs ...)))))

(define-syntax pmatch1
  (syntax-rules (else guard)
    ((_ v) (if #f #f))
    ((_ v (else e0 e ...)) (let () e0 e ...))
    ((_ v (pat (guard g ...) e0 e ...) cs ...)
     (let ((fk (lambda () (pmatch1 v cs ...))))
       (ppat v pat
             (if (and g ...) (let () e0 e ...) (fk))
             (fk))))
    ((_ v (pat e0 e ...) cs ...)
     (let ((fk (lambda () (pmatch1 v cs ...))))
       (ppat v pat (let () e0 e ...) (fk))))))

(define-syntax ppat
  (syntax-rules (_ quote unquote)
    ((_ v _ kt kf) kt)
    ((_ v () kt kf) (if (null? v) kt kf))
    ((_ v (quote lit) kt kf)
     (if (equal? v (quote lit)) kt kf))
    ((_ v (unquote var) kt kf) (let ((var v)) kt))
    ((_ v (x . y) kt kf)
     (if (pair? v)
         (let ((vx (car v)) (vy (cdr v)))
           (ppat vx x (ppat vy y kt kf) kf))
         kf))
    ((_ v lit kt kf) (if (eq? v (quote lit)) kt kf))))

;; this works for some but not for lambda-case in srfi-16
(define-syntax include-from-path
  (syntax-rules ()
    ((_ file)
     (let* ((env (current-module))
	    (path (%search-load-path file))
	    (port (open-input-file path)))
       (let loop ((exp (read port)))
	 (cond
	  ((eof-object? exp) (if #f #f))
	  (else
	   (eval exp env)
	   (loop (read port)))))))))

;;; --- last line ---

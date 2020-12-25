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
;;; cache.scm --- cache the results of parsing

(define-module (ice-9 peg cache)
  #:export (cg-cached-parser))

;; The results of parsing using a nonterminal are cached.  Think of it like a
;; hash with no conflict resolution.  Process for deciding on the cache size
;; wasn't very scientific; just ran the benchmarks and stopped a little after
;; the point of diminishing returns on my box.
(define *cache-size* 512)

(define (make-cache)
  (make-vector *cache-size* #f))

;; given a syntax object which is a parser function, returns syntax
;; which, if evaluated, will become a parser function that uses a cache.
(define (cg-cached-parser parser)
  #`(let ((cache (make-cache)))
      (lambda (str strlen at)
        (let* ((vref (vector-ref cache (modulo at *cache-size*))))
          ;; Check to see whether the value is cached.
          (if (and vref (eq? (car vref) str) (= (cadr vref) at))
              (caddr vref);; If it is return it.
              (let ((fres ;; Else calculate it and cache it.
                     (#,parser str strlen at)))
                (vector-set! cache (modulo at *cache-size*)
                             (list str at fres))
                fres))))))

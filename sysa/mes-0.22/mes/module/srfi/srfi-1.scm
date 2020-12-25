;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc.
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

;;; From Guile-1.8

;;; Author: Martin Grabmueller <mgrabmue@cs.tu-berlin.de>
;;; Date: 2001-06-06

;;; Searching

;; Internal helper procedure.  Map `f' over the single list `ls'.
;;
(define map1 map)

(define (any pred ls . lists)
  (if (null? lists)
      (any1 pred ls)
      (let lp ((lists (cons ls lists)))
	(cond ((any1 null? lists)
	       #f)
	      ((any1 null? (map1 cdr lists))
	       (apply pred (map1 car lists)))
	      (else
	       (or (apply pred (map1 car lists)) (lp (map1 cdr lists))))))))

(define (any1 pred ls)
  (let lp ((ls ls))
    (cond ((null? ls)
	   #f)
	  ((null? (cdr ls))
	   (pred (car ls)))
	  (else
	   (or (pred (car ls)) (lp (cdr ls)))))))

(define (every pred ls . lists)
  (if (null? lists)
      (every1 pred ls)
      (let lp ((lists (cons ls lists)))
	(cond ((any1 null? lists)
	       #t)
	      ((any1 null? (map1 cdr lists))
	       (apply pred (map1 car lists)))
	      (else
	       (and (apply pred (map1 car lists)) (lp (map1 cdr lists))))))))

(define (every1 pred ls)
  (let lp ((ls ls))
    (cond ((null? ls)
	   #t)
	  ((null? (cdr ls))
	   (pred (car ls)))
	  (else
	   (and (pred (car ls)) (lp (cdr ls)))))))

(define (list-index pred clist1 . rest)
  (if (null? rest)
    (let lp ((l clist1) (i 0))
      (if (null? l)
	#f
	(if (pred (car l))
	  i
	  (lp (cdr l) (+ i 1)))))
    (let lp ((lists (cons clist1 rest)) (i 0))
      (cond ((any1 null? lists)
	     #f)
	    ((apply pred (map car lists)) i)
	    (else
	     (lp (map cdr lists) (+ i 1)))))))

;;; Set operations on lists

(define (lset-union = . rest)
  (let ((acc '()))
    (for-each (lambda (lst)
		(if (null? acc)
		    (set! acc lst)
		    (for-each (lambda (elem)
				(if (not (member elem acc =))
				    (set! acc (cons elem acc))))
			      lst)))
	      rest)
    acc))

(define (lset-intersection = list1 . rest)
  (let lp ((l list1) (acc '()))
    (if (null? l)
      (reverse! acc)
      (if (every (lambda (ll) (member (car l) ll =)) rest)
	(lp (cdr l) (cons (car l) acc))
	(lp (cdr l) acc)))))

(define (lset-difference = list1 . rest)
  (if (null? rest)
    list1
    (let lp ((l list1) (acc '()))
      (if (null? l)
	(reverse! acc)
	(if (any (lambda (ll) (member (car l) ll =)) rest)
	  (lp (cdr l) acc)
	  (lp (cdr l) (cons (car l) acc)))))))

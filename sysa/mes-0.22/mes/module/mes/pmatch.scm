;;; pmatch, a simple matcher

;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright (C) 2009, 2010, 2012 Free Software Foundation, Inc
;;; Copyright (C) 2005,2006,2007 Oleg Kiselyov
;;; Copyright (C) 2007 Daniel P. Friedman
;;; Copyright (C) 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

;;; Originally written by Oleg Kiselyov for LeanTAP in Kanren, which is
;;; available under the MIT license.
;;;
;;; http://kanren.cvs.sourceforge.net/viewvc/kanren/kanren/mini/leanTAP.scm?view=log
;;;
;;; This version taken from:
;;; αKanren: A Fresh Name in Nominal Logic Programming
;;; by William E. Byrd and Daniel P. Friedman
;;; Proceedings of the 2007 Workshop on Scheme and Functional Programming
;;; Université Laval Technical Report DIUL-RT-0701

;;; To be clear: the original code is MIT-licensed, and the modifications
;;; made to it by Guile are under Guile's license (currently LGPL v3+).

;;; Code:

;; (pmatch exp <clause> ...[<else-clause>])
;; <clause> ::= (<pattern> <guard> exp ...)
;; <else-clause> ::= (else exp ...)
;; <guard> ::= boolean exp | ()
;; <pattern> :: =
;;        ,var  -- matches always and binds the var
;;                 pattern must be linear! No check is done
;;         _    -- matches always
;;        'exp  -- comparison with exp (using equal?)    REMOVED (August 8, 2012)
;;        exp   -- comparison with exp (using equal?)
;;        (<pattern1> <pattern2> ...) -- matches the list of patterns
;;        (<pattern1> . <pattern2>)  -- ditto
;;        ()    -- matches the empty list

(define-module (system base pmatch)
  #:export-syntax (pmatch))

(define-syntax pmatch
  (syntax-rules (else guard)
    ((_ v) (if #f #f))
    ((_ v (else e0 e ...)) (let () e0 e ...))
    ((_ v (pat (guard g ...) e0 e ...) cs ...)
     (let ((fk (lambda () (pmatch v cs ...))))
       (ppat v pat
             (if (and g ...) (let () e0 e ...) (fk))
             (fk))))
    ((_ v (pat e0 e ...) cs ...)
     (let ((fk (lambda () (pmatch v cs ...))))
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
         (ppat (pmatch-car v) x (ppat (pmatch-cdr v) y kt kf) kf)
         kf))
    ((_ v lit kt kf) (if (eq? v (quote lit)) kt kf))))

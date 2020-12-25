;;; GNU Mes --- Maxwell Equations of Software
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

(define-module (mes misc)
  #:use-module (srfi srfi-1)
  #:export (%scheme
            disjoin
            guile?
            mes?
            pk
            pke
            warn
            stderr
            string-substitute))

(cond-expand
 (mes
  (define %scheme "mes"))
 (guile
  (define %scheme "guile")))

(define guile? (equal? %scheme "guile"))
(define mes? (equal? %scheme "mes"))

(define (logf port string . rest)
  (apply format (cons* port string rest))
  (force-output port)
  #t)

(define (stderr string . rest)
  (apply logf (cons* (current-error-port) string rest)))

(define (pk . stuff)
  (newline)
  (display ";;; ")
  (write stuff)
  (newline)
  (car (last-pair stuff)))

(define (pke . stuff)
  (display "\n" (current-error-port))
  (newline (current-error-port))
  (display ";;; " (current-error-port))
  (write stuff (current-error-port))
  (display "\n" (current-error-port))
  (car (last-pair stuff)))

(define warn pke)

(define (disjoin . predicates)
  (lambda (. arguments)
    (any (lambda (o) (apply o arguments)) predicates)))

(define (string-substitute string find replace)
  (let ((index (string-contains string find)))
    (if (not index) string
        (string-append
         (string-take string index)
         replace
         (string-substitute
          (string-drop string (+ index (string-length find)))
          find replace)))))

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

;;; Commentary:

;;; Code:

(define-module (mescc bytevectors)
  #:use-module (mes guile)
  #:export (bytevector-u64-native-set!
            bytevector-u32-native-set!
            bytevector-u16-native-set!
            bytevector-u8-set!
            make-bytevector))

;; rnrs compatibility
(define (bytevector-u64-native-set! bv index value)
  (when (not (= 0 index)) (error "bytevector-u64-native-set! index not zero: " index " value: " value))
  (let ((x (list
            (modulo value #x100)
            (modulo (ash value -8) #x100)
            (modulo (ash value -16) #x100)
            (modulo (ash value -24) #x100)
            (modulo (ash value -32) #x100)
            (modulo (ash value -40) #x100)
            (modulo (ash value -48) #x100)
            (modulo (ash value -56) #x100))))
    (set-car! bv (car x))
    (set-cdr! bv (cdr x))
    x))

(define (bytevector-u32-native-set! bv index value)
  (when (not (= 0 index)) (error "bytevector-u32-native-set! index not zero: " index " value: " value))
  (let ((x (list
            (modulo value #x100)
            (modulo (ash value -8) #x100)
            (modulo (ash value -16) #x100)
            (modulo (ash value -24) #x100))))
    (set-car! bv (car x))
    (set-cdr! bv (cdr x))
    x))

(define (bytevector-u16-native-set! bv index value)
  (when (not (= 0 index)) (error "bytevector-u16-native-set! index not zero: " index " value: " value))
  (let ((x (list
            (modulo value #x100)
            (modulo (ash value -8) #x100))))
    (set-car! bv (car x))
    (set-cdr! bv (cdr x))
    x))

(define (bytevector-u8-set! bv index value)
  (when (not (= 0 index)) (error "bytevector-u8-set! index not zero: " index " value: " value))
  (let ((x (modulo value #x100)))
    (set-car! bv x)
    x))

(define (make-bytevector length)
  (make-list length 0))

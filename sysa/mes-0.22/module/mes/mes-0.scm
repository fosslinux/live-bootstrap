;;; -*-scheme-*-

;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright © 2016,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;;
;;; mes-0.scm: This file is part of GNU Mes.
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

;;; mes-0.scm is the first file being loaded into Guile.  It provides
;;; non-standard definitions that Mes modules and tests depend on.

;;; Code:

(define-module (mes mes-0)
  #:export (
            builtin?
            mes-use-module
            EOF
            append2
            mes?
            guile?
            guile-1.8?
            guile-2?
            %arch
            %compiler
            ))
(cond-expand
 (guile-2)
 (guile
  (define %host-type (string-append (utsname:machine (uname)) "linux-gnu")))
 (else))

(define-macro (mes-use-module . rest) #t)
(define builtin? procedure?) ; not strictly true, but ok for tests/*.test
(define mes? #f)
(define guile? #t)
(define guile-1.8? (equal? (effective-version) "1.8"))
(define guile-2? (equal? (major-version) "2"))
(define EOF (if #f #f))
(define append2 append)
(define %arch (car (string-split %host-type #\-)))
(define %compiler "gnuc")

;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(define display core:display)
(define write core:write)

;; unmemoize removes formal caching...but only one level
(define (foo doit bar)
  (define baz
    (lambda (doit)
      (display "   baz:doit=")
      (write doit)
      (display "   baz:bar=")
      (write bar)
      (display "\n")
      (doit bar)))
  (display "foo doit=")
  (write doit)
  (display "\n")
  (display " bar=")
  (write bar)
  (display "\n")
  (display "  baz=")
  (write baz)
  (display "\n")
  (baz doit))

(foo display 1)
(display "foo=")
(write foo)
(display "\n")
(foo exit 0)
(exit 1)

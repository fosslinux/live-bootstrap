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

((lambda (port)
   (core:display-error "port:")
   (core:write-error port)
   (core:display-error "\n")
   (set-current-input-port port)
   (core:display-error "current:")
   (core:write-error (current-input-port))
   (core:display-error "\n")
   (core:display-error "read:")
   ((lambda (string)
      (core:write-error string)
      (core:display-error "\n")
      (core:display-error "empty:")
      (core:write-error port)
      (core:display-error "\n")
      (exit (if (equal2? string "foo bar\n") 0 1)))
    ((if (pair? (current-module)) read-string (@ (ice-9 rdelim) read-string)) port)))
 (open-input-string "foo bar\n"))

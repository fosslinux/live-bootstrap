#! /bin/sh
# -*-scheme-*-
mes_p=$(command -v mes)
if [ "$mes_p" -a -z "$MES" ]; then
    MES=guile
fi
exec ${MES-mes} -L ${0%/*} -e '(diff)' -s "$0" "$@"
!#

;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;;
;;; mes-snarf.scm: This file is part of GNU Mes.
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

(define-module (diff)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 rdelim)
  #:export (main))

(cond-expand
 (mes
  (define %scheme "mes"))
 (guile-2
  (define %scheme "guile")
  (define-macro (mes-use-module . rest) #t))
 (guile
  (use-modules (ice-9 syncase))
  (define %scheme "guile")
  (define-macro (mes-use-module . rest) #t)))

(mes-use-module (mes guile))

(format (current-error-port) "diff[~a]...\n" %scheme)

(define (plus a)
  (string-append "+" a))
(define (minus a)
  (string-append "-" a))
(define (keep a)
  (string-append " " a))

(define-record-type <hunk> (make-hunk context after removed added)
  hunk?
  (context hunk.context)
  (after hunk.after)
  (removed hunk.removed)
  (added hunk.added))

(define (hunk->lines o)
  (append `(,(format #f "@@ -~a,~a +~a,~a" (length (hunk.removed o)) (+ 3 (car (hunk.context o))) (length (hunk.added o)) (+ 3 (cadr (hunk.context o))))
            ,@(map keep (filter identity (cddr (hunk.context o)))))
          (map minus (hunk.removed o))
          (map plus (hunk.added o))
          (map keep (hunk.after o))))

(define (safe-list-head lst n)
  (list-head lst (min n (length lst))))

(define (line-equal? a b)
  (equal? (string-trim-right a) (string-trim-right b)))

(define (diff-files a b)
  (let ((a-lines (string-split (with-input-from-file a read-string) #\newline))
        (b-lines (string-split (with-input-from-file b read-string) #\newline)))
    (let loop ((context '(1 1 #f #f #f)) (a-lines a-lines) (b-lines b-lines))
      ;;(format (current-error-port) "loop context=~s\n" context)
      (cond ((and (null? a-lines) (null? b-lines)) '())
            ((null? a-lines)
             (list (make-hunk context (safe-list-head a-lines 3) '() b-lines)))
            ((null? b-lines)
             (list (make-hunk context (safe-list-head a-lines 3) a-lines '())))
            ((line-equal? (car a-lines) (car b-lines))
             (loop `(,(1+ (car context))
                     ,(1+ (cadr context))
                     ,@(cdddr context)
                     ,(car a-lines))
                   (cdr a-lines) (cdr b-lines)))
            (else
             (cond ((and (pair? (cdr b-lines)) (line-equal? (car a-lines) (cadr b-lines)))
                    (cons (make-hunk context (safe-list-head a-lines 3) '() (list (car b-lines)))
                          (loop `(,(+ 1 (car context))
                                  ,(+ 2 (cadr context))
                                  ,@(cdddr context)
                                  ,(car a-lines))
                                (cdr a-lines) (cddr b-lines))))
                   ((and (pair? (cdr a-lines)) (line-equal? (cadr a-lines) (car b-lines)))
                    (cons (make-hunk context (safe-list-head a-lines 3) (list (car a-lines)) '())
                          (loop `(,(+ 2 (car context))
                                  ,(+ 1 (cadr context))
                                  ,@(cddddr context)
                                  ,(car a-lines)
                                  ,(cadr a-lines))
                                (cddr a-lines) (cdr b-lines))))
                   (else (cons (make-hunk context (safe-list-head a-lines 3) (list (car a-lines)) (list (car b-lines)))
                               (loop `(,(1+ (car context))
                                       ,(1+ (cadr context))
                                       ,@(cdddr context)
                                       ,(car a-lines))
                                     (cdr a-lines) (cdr b-lines))))))))))

(define (main args)
  (let ((files (cdr args)))
    (when (or (null? files)
              (equal? (car files) "-h")
              (equal? (car files) "--help"))
      (display "
Usage: diff.scm [OPTION]... FILES
Compare FILES line by line.

Options:
  -u,--unified        display unified diff (default)
  -h,--help           display this help and exit
  -V,--version        display version information and exit
")
      (exit (if (null? files) 2 0)))
    (when (or (equal? (car files) "-V")
              (equal? (car files) "--version"))
      (display "
diff.scm (GNU Mes) 0.20
"))
    (let* ((files (if (string-prefix? "-" (car files)) (cdr files) files))
           (hunks (apply diff-files (list-head files 2))))
     (when (pair? hunks)
       (display (string-join (append-map hunk->lines hunks) "\n"))
       (newline)
       (exit 1)))))

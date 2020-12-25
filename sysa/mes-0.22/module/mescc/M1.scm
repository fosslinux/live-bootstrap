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

;;; M1.scm produces stage0' M1 assembly format

;;; Code:

(define-module (mescc M1)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (system base pmatch)
  #:use-module (mes misc)
  #:use-module (mes guile)

  #:use-module (mescc as)
  #:use-module (mescc info)
  #:export (info->M1
            infos->M1
            M1:merge-infos))

(define* (infos->M1 file-name infos #:key align? verbose?)
  (let ((info (fold M1:merge-infos (make <info>) infos)))
    (info->M1 file-name info #:align? align? #:verbose? verbose?)))

(define (M1:merge-infos o info)
  (clone info
         #:functions (alist-add (.functions info) (.functions o))
         #:globals (alist-add (.globals info) (.globals o))
         #:types (.types o)))

(define (alist-add a b)
  (let* ((b-keys (map car b))
         (a (filter (lambda (f) (or (cdr f) (not (member (car f) b-keys)))) a))
         (a-keys (map car a)))
    (append a (filter (lambda (e) (not (member (car e) a-keys))) b))))

(define (hex2:address o)
  (string-append "&" o))

(define (hex2:address8 o)
  (string-append "&" o " %0")) ;; FIXME: 64bit

(define (hex2:offset o)
  (string-append "%" o))

(define (hex2:offset1 o)
  (string-append "!" o))

(define hex? #t)

(define (hex2:immediate o)
  (if hex? (string-append "%0x" (dec->hex o))
      (string-append "%" (number->string o))))

(define (hex2:immediate1 o)
  (if hex? (string-append "!0x" (dec->hex o))
      (string-append "!" (number->string o))))

(define (hex2:immediate2 o)
  (if hex? (string-append "@0x" (dec->hex o))
      (string-append "@" (number->string o))))

(define (hex2:immediate4 o)
  (if hex? (string-append "%0x" (dec->hex o))
      (string-append "%" (number->string o))))

(define mesc? (string=? %compiler "mesc"))

(define (hex2:immediate8 o)
  ;; FIXME: #x100000000 => 0 divide-by-zero when compiled with 64 bit mesc
  (if hex? (string-append "%0x" (dec->hex (if mesc? 0 (modulo o #x100000000)))
                          " %0x" (if (< o 0) "-1"
                                     (dec->hex (if mesc? o (quotient o #x100000000)))))
      (string-append "%" (number->string (dec->hex (if mesc? 0 (modulo o #x100000000))))
                     " %" (if (< o 0) "-1"
                              (number->string (dec->hex (if mesc? o (quotient o #x100000000))))))))

(define* (display-join o #:optional (sep ""))
  (let loop ((o o))
    (when (pair? o)
      (display (car o))
      (if (pair? (cdr o))
          (display sep))
      (loop (cdr o)))))

(define (global-string? o)
  (and (pair? o) (pair? (car o)) (eq? (caar o) #:string)))

(define (global-extern? o)
  (and=> (global:storage o) (cut eq? <> 'extern)))

(define* (info->M1 file-name o #:key align? verbose?)
  (let* ((functions (.functions o))
         (function-names (map car functions))
         (globals (.globals o))
         (globals (filter (negate (compose global-extern? cdr)) globals))
         (strings (filter global-string? globals))
         (strings (map car strings))
         (reg-size (type:size (assoc-ref (.types o) "*"))))
    (define (string->label o)
      (let ((index (list-index (lambda (s) (equal? s o)) strings)))
        (if index
            (string-append "_string_" file-name "_" (number->string index))
            (if (equal? o "%0") o       ; FIXME: 64b
                (error "no such string:" o)))))
    (define (text->M1 o)
      ;;
      (cond
       ((char? o) (text->M1 (char->integer o)))
       ((string? o) o)
       ((symbol? o) (symbol->string o))
       ((number? o) (let ((o (if (< o #x80) o (- o #x100))))
                      (if hex? (string-append "!0x"
                                              (if (and (>= o 0) (< o 16)) "0" "")
                                              (number->string o 16))
                          (string-append "!" (number->string o)))))
       ((and (pair? o) (keyword? (car o)))
        (pmatch o
          ;; FIXME
          ((#:address (#:string ,string))
           (hex2:address (string->label `(#:string ,string))))
          ((#:address (#:address ,address)) (guard (string? address))
           (hex2:address address))
          ((#:address (#:address ,global)) (guard (global? global))
           (hex2:address (global->string global)))
          ((#:address ,function) (guard (function? function))
           (hex2:address (function->string function)))
          ((#:address ,number) (guard (number? number))
           (string-join (map text->M1 (int->bv32 number))))

          ((#:address8 (#:string ,string))
           (hex2:address8 (string->label `(#:string ,string))))
          ((#:address8 (#:address ,address)) (guard (string? address))
           (hex2:address8 address))
          ((#:address8 (#:address ,global)) (guard (global? global))
           (hex2:address8 (global->string global)))
          ((#:address8 ,function) (guard (function? function))
           (hex2:address8 (function->string function)))
          ((#:address8 ,number) (guard (number? number))
           (string-join (map text->M1 (int->bv64 number))))

          ((#:string ,string)
           (hex2:address (string->label o)))

          ((#:address ,address) (guard (string? address))
           (hex2:address address))
          ((#:address ,global) (guard (global? global))
           (hex2:address (global->string global)))

          ((#:address8 ,address) (guard (string? address))
           (hex2:address8 address))
          ((#:address8 ,global) (guard (global? global))
           (hex2:address8 (global->string global)))

          ((#:offset ,offset) (hex2:offset offset))
          ((#:offset1 ,offset1) (hex2:offset1 offset1))
          ((#:immediate ,immediate) (hex2:immediate immediate))
          ((#:immediate1 ,immediate1) (hex2:immediate1 immediate1))
          ((#:immediate2 ,immediate2) (hex2:immediate2 immediate2))
          ((#:immediate4 ,immediate4) (hex2:immediate4 immediate4))
          ((#:immediate8 ,immediate8) (hex2:immediate8 immediate8))
          (_ (error "text->M1 no match o" o))))
       ((pair? o) (string-join (map text->M1 o)))
       (#t (error "no such text:" o))))
    (define (write-function o)
      (let ((name (car o))
            (text (function:text (cdr o))))
        (define (line->M1 o)
          (cond ((eq? (car o) #:label)
                 (display (string-append ":" (cadr o))))
                ((eq? (car o) #:comment)
                 (display "\t\t\t\t\t# ")
                 (display (text->M1 (cadr o))))
                ((or (string? (car o)) (symbol? (car o)))
                 (display "\t" )
                 (display-join (map text->M1 o) " "))
                (else (error "line->M1 invalid line:" o)))
          (newline))
        (when verbose?
          (display (string-append "    :" name "\n") (current-error-port)))
        (display (string-append "\n\n:" name "\n"))
        (for-each line->M1 (apply append text))))
    (define (write-global o)
      (define (labelize o)
        (if (not (string? o)) o
            (let* ((label o)
                   (function? (member label function-names))
                   (string-label (string->label label))
                   (string? (not (equal? string-label "_string_#f"))))
              (cond ((and (pair? o) (global? (cdr o))) (string-append "&" (global->string o)))
                    ((and (not string?) (not function?)) (stderr "warning: unresolved label: ~s\n" label))
                    ((equal? string-label "%0") o) ;; FIXME: 64b
                    (else (string-append "&" label))))))
      (define (display-align size)
        (let ((alignment (- reg-size (modulo size reg-size))))
          (when (and align? (> reg-size alignment 0))
            (display " ")
            (display-join (map text->M1 (map (const 0) (iota alignment))) " "))
          #t))
      (let* ((label (cond
                     ((and (pair? (car o)) (eq? (caar o) #:string))
                      (string->label (car o)))
                     ((global? (cdr o)) (global->string (cdr o)))
                     (else (car o))))
             (string? (string-prefix? "_string" label))
             (foo (when (and verbose? (not (eq? (car (string->list label)) #\_)))
                    (display (string-append "    :" label "\n") (current-error-port))))
             (data ((compose global:value cdr) o))
             (data (filter-map labelize data))
             (len (length data))
             (string-max (or (and=> (getenv "M1_STRING_MAX") string->number) 256))
             (string-data (and string? (list-head data (1- (length data))))))
        (display (string-append "\n:" label "\n"))
        (if (and string-data
                 (< len string-max)
                 (char? (car data))
                 (eq? (last data) #\nul)
                 (not (find (cut memq <> '(#\")) string-data))
                 (not (any (lambda (ch)
                             (or (and (not (memq ch '(#\tab #\newline)))
                                      (< (char->integer ch) #x20))
                                 (>= (char->integer ch) #x80))) string-data)))
            (let ((text string-data))
              (display (string-append "\"" (list->string string-data) "\""))
              (display-align (1+ (length string-data))))
            (let ((text (map text->M1 data)))
              (display-join  text " ")
              (display-align (length text))))
        (newline)))
    (when verbose?
      (display "M1: functions\n" (current-error-port)))
    (for-each write-function (filter cdr functions))
    (when (assoc-ref functions "main")
      (display "\n\n:ELF_data\n") ;; FIXME
      (display "\n\n:HEX2_data\n"))
    (when verbose?
      (display "M1: globals\n" (current-error-port)))
    (for-each write-global (filter global-string? globals))
    (for-each write-global (filter (negate global-string?) globals))))

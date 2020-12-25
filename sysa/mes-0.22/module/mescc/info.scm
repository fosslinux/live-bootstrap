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

;;; info.scm defines [Guile] record data types for MesCC

;;; Code:

(define-module (mescc info)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:export (<info>
            make
            clone
            make-<info>
            info?

            .types
            .constants
            .functions
            .globals
            .locals
            .function
            .statics
            .text
            .post
            .break
            .continue
            .allocated
            .pushed
            .registers
            .instructions

            <type>
            make-type
            type?
            type:type
            type:size
            type:pointer
            type:description

            <c-array>
            make-c-array
            c-array?
            c-array:type
            c-array:count

            <pointer>
            make-pointer
            pointer?
            pointer:type
            pointer:rank

            <bit-field>
            make-bit-field
            bit-field?
            bit-field:type
            bit-field:bit
            bit-field:bits

            <var>
            var:name
            var:type
            var:pointer
            var:c-array

            <global>
            make-global
            global?
            global:name
            global:type
            global:pointer
            global:c-array
            global:var
            global:value
            global:storage
            global:function
            global->string

            <local>
            make-local
            local?
            local:type
            local:pointer
            local:c-array
            local:var
            local:id

            <function>
            make-function
            function?
            function:name
            function:type
            function:text
            function->string

            ->type
            ->rank
            rank--
            rank++
            rank+=
            structured-type?))

(define-immutable-record-type <info>
  (make-<info> types constants functions globals locals statics function text post break continue allocated pushed registers instructions)
  info?
  (types .types)
  (constants .constants)
  (functions .functions)
  (globals .globals)
  (locals .locals)
  (statics .statics)
  (function .function)
  (text .text)
  (post .post)
  (break .break)
  (continue .continue)
  (allocated .allocated)
  (pushed .pushed)
  (registers .registers)
  (instructions .instructions))

(define* (make o #:key (types '()) (constants '()) (functions '()) (globals '()) (locals '()) (statics '()) (function #f) (text '()) (post '()) (break '()) (continue '()) (allocated '()) (pushed 0) (registers '()) (instructions '()))
  (cond ((eq? o <info>)
         (make-<info> types constants functions globals locals statics function text post break continue allocated  pushed registers instructions))))

(define (clone o . rest)
  (cond ((info? o)
         (let ((types (.types o))
               (constants (.constants o))
               (functions (.functions o))
               (globals (.globals o))
               (locals (.locals o))
               (statics (.statics o))
               (function (.function o))
               (text (.text o))
               (post (.post o))
               (break (.break o))
               (continue (.continue o))
               (allocated (.allocated o))
               (pushed (.pushed o))
               (registers (.registers o))
               (instructions (.instructions o)))
           (let-keywords rest
                         #f
                         ((types types)
                          (constants constants)
                          (functions functions)
                          (globals globals)
                          (locals locals)
                          (statics statics)
                          (function function)
                          (text text)
                          (post post)
                          (break break)
                          (continue continue)
                          (allocated allocated)
                          (pushed pushed)
                          (registers registers)
                          (instructions instructions))
                         (make <info> #:types types #:constants constants #:functions functions #:globals globals  #:locals locals #:statics statics #:function function #:text text #:post post #:break break #:continue continue #:allocated allocated #:pushed pushed #:registers registers #:instructions instructions))))))

;; ("int" . ,(make-type 'builtin 4 #f 0 #f))
;;           (make-type 'enum 4 0 fields)
;;           (make-type 'struct (apply + (map field:size fields)) 0 fields)

(define-immutable-record-type <type>
  (make-type type size description)
  type?
  (type type:type)
  (size type:size)
  (description type:description))

(define-immutable-record-type <c-array>
  (make-c-array type count)
  c-array?
  (type c-array:type)
  (count c-array:count))

(define-immutable-record-type <pointer>
  (make-pointer type rank)
  pointer?
  (type pointer:type)
  (rank pointer:rank))

(define-immutable-record-type <bit-field>
  (make-bit-field type bit bits)
  bit-field?
  (type bit-field:type)
  (bit bit-field:bit)
  (bits bit-field:bits))

(define-immutable-record-type <var>
  (make-var name type function id value)
  var?
  (name var:name)
  (type var:type)                       ; <type>
  (function var:function)
  (id var:id)
  (value var:value))

(define-immutable-record-type <global>
  (make-global- name type var value storage function)
  global?
  (name global:name)
  (type global:type)
  (var global:var)                      ; <var>

  (value global:value)
  (storage global:storage)
  (function global:function))

(define (make-global name type value storage function)
  (make-global- name type (make-var name type function #f value) value storage function))

(define (global->string o)
  (or (and=> (global:function o) (cut string-append <> "-" (global:name o)))
      (global:name o)))

(define-immutable-record-type <local>
  (make-local- type var id)
  local?
  (type local:type)
  (var local:var)                       ; <var>

  (id local:id))

(define (make-local name type id)
  (make-local- type (make-var name type #f id #f) id))

(define-immutable-record-type <function>
  (make-function name type text)
  function?
  (name function:name)
  (type function:type)
  (text function:text))

(define (function->string o)
  (function:name o))

(define (structured-type? o)
  (cond ((type? o) (memq (type:type o) '(struct union)))
        ((global? o) ((compose structured-type? global:type) o))
        ((local? o) ((compose structured-type? local:type) o))
        ((and (pair? o) (eq? (car o) 'tag))) ;; FIXME: enum?
        (else #f)))

(define (->type o)
  (cond ((type? o) o)
        ((bit-field? o) o)
        ((pointer? o) ((compose ->type pointer:type) o))
        ((c-array? o) ((compose ->type c-array:type) o))
        ((and (pair? o) (eq? (car o) 'tag)) o)
        ;; FIXME
        (#t
         (format (current-error-port) "->type--: not a <type>: ~s\n" o)
         (make-type 'builtin 4 #f))
        (else (error "->type: not a <type>:" o))))

(define (->rank o)
  (cond ((type? o) 0)
        ((pointer? o) (pointer:rank o))
        ((c-array? o) (1+ ((compose ->rank c-array:type) o)))
        ((local? o) ((compose ->rank local:type) o))
        ((global? o) ((compose ->rank global:type) o))
        ((bit-field? o) 0)
        ;; FIXME
        (#t
         (format (current-error-port) "->rank: not a type: ~s\n" o)
         0)
        (else (error "->rank: not a <type>:" o))))

(define (rank-- o)
  (cond ((and (pointer? o) (= (pointer:rank o) 1)) (pointer:type o))
        ((pointer? o) (set-field o (pointer:rank) (1- (pointer:rank o))))
        ((c-array? o) (c-array:type o))
        ;; FIXME
        (#t (format (current-error-port) "rank--: not a pointer: ~s\n" o)
              o)
        (else (error "rank--: not a pointer" o))))

(define (rank+= o i)
  (cond ((pointer? o) (set-field o (pointer:rank) (+ i (pointer:rank o))))
        (else (make-pointer o i))))

(define (rank++ o)
  (rank+= o 1))

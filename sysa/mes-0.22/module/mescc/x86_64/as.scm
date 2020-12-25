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

;;; Commentary:

;;; Define x86_64 M1 assembly

;;; Code:

(define-module (mescc x86_64 as)
  #:use-module (mes guile)
  #:use-module (mescc as)
  #:use-module (mescc info)
  #:use-module (mescc x86_64 info)
  #:export (
            x86_64:instructions
            ))

(define (r->e o)
  (string-append "e" (string-drop o 1)))
(define (r->x o)
  (string-drop o 1))
(define (r->l o)
  (assoc-ref
   '(("rax" . "al")
     ("rdi" . "dil")
     ("rsi" . "sil")
     ("rdx" . "dl")
     ("rcx" . "cl")
     ("r8" . "r8b")
     ("r9" . "r9b"))
   o))

;; AMD
(define (x86_64:function-preamble info . rest)
  `(("push___%rbp")
    ("mov____%rsp,%rbp")
    ("sub____$i32,%rbp" "%0x80")
    ,@(list-head
       '(("mov____%rdi,0x8(%rbp)" "!0x10")
         ("mov____%rsi,0x8(%rbp)" "!0x18")
         ("mov____%rdx,0x8(%rbp)" "!0x20")
         ("mov____%rcx,0x8(%rbp)" "!0x28")
         ("mov____%r8,0x8(%rbp)" "!0x30")
         ("mov____%r9,0x8(%rbp)" "!0x38"))
       (length (car rest)))))

;; traditional
(define (x86_64:function-preamble info . rest)
  `(("push___%rbp")
    ("mov____%rsp,%rbp")))

(define (x86_64:function-locals . rest)
  `(
    ;; FIXME: how on x86_64?
    ("sub____$i32,%rsp" (#:immediate ,(+ (* 4 1025) (* 20 8))))
    )) ; 4*1024 buf, 20 local vars

(define (x86_64:r->local info n)
  (let ((r (get-r info))
        (n (- 0 (* 8 n))))
    `(,(if (< (abs n) #x80)
           `(,(string-append "mov____%" r ",0x8(%rbp)") (#:immediate1 ,n))
           `(,(string-append "mov____%" r ",0x32(%rbp)") (#:immediate ,n))))))

(define (x86_64:value->r info v)
  (or v (error "invalid value: x86_64:value->r: " v))
  (let ((r (get-r info)))
    (if (and (>= v 0)
             (< v #xffffffff))
     `((,(string-append "mov____$i32,%" r) (#:immediate ,v)))
     `((,(string-append "mov____$i64,%" r) (#:immediate8 ,v))))))

;; AMD
(define (x86_64:ret . rest)
  '(("add____$i32,%rbp" "%0x80")
    ("mov____%rbp,%rsp")
    ("pop____%rbp")
    ("ret")))

;; traditional
(define (x86_64:ret . rest)
  '(("mov____%rbp,%rsp")
    ("pop____%rbp")
    ("ret")))

(define (x86_64:r-zero? info)
  (let ((r (car (if (pair? (.allocated info)) (.allocated info) (.registers info)))))
    `((,(string-append "test___%" r "," "%" r)))))

(define (x86_64:local->r info n)
  (let ((r (car (if (pair? (.allocated info)) (.allocated info) (.registers info))))
        (n (- 0 (* 8 n))))
    `(,(if (< (abs n) #x80) `(,(string-append "mov____0x8(%rbp),%" r) (#:immediate1 ,n))
           `(,(string-append "mov____0x32(%rbp),%" r) (#:immediate ,n))))))

(define (x86_64:call-label info label n)
  `((call32 (#:offset ,label))
    ("add____$i8,%rsp" (#:immediate1 ,(* n 8)))  ;; NOT AMD
    ))

(define x86_64:calling-convention-registers '("rax" "rdi" "rsi" "rdx" "rcx" "r8" "r9"))

;; AMD
(define (x86_64:r->arg info i)
  (let ((r (get-r info))
        (r1 (list-ref x86_64:calling-convention-registers (1+ i))))
    `((,(string-append "mov____%" r ",%" r1))))) ; debug fail-safe check

(define (x86_64:label->arg info label i)
  (let ((r0 (list-ref x86_64:registers (1+ i))))
    (if (< label #x80000000)
        `((,(string-append "mov____$i32,%" r0) (#:address ,label)))
        `((,(string-append "mov____$i64,%" r0) (#:address8 ,label))))))

;; traditional
(define (x86_64:r->arg info i)
  (let ((r (get-r info)))
    `((,(string-append "push___%" r)))))

(define (x86_64:label->arg info label i)
  `(("push___$i32" (#:address ,label))))

;; FIXME?
;; (define (x86_64:label->arg info label i)
;;   `((,(string-append "mov____$i64,%r15") (#:address8 ,label))
;;     ("push___%r15" (#:address ,label))))

(define (x86_64:r0+r1 info)
  (let ((r1 (get-r1 info))
        (r0 (get-r0 info)))
    `((,(string-append "add____%" r1 ",%" r0)))))

(define (x86_64:r-negate info)
  (let* ((r (get-r info))
         (l (r->l r)))
    `((,(string-append "sete___%" l))
      (,(string-append "movzbq_%" l ",%" r)))))

(define (x86_64:r0-r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "sub____%" r1 ",%" r0)))))

(define (x86_64:zf->r info)
  (let* ((r (get-r info))
         (l (r->l r)))
    `((,(string-append "sete___%" l))
      (,(string-append "movzbq_%" l ",%" r)))))

(define (x86_64:xor-zf info)
  '(("lahf")
    ("xor____$i8,%ah" (#:immediate1 #x40))
    ("sahf")))

(define (x86_64:r->local+n info id n)
  (let ((n (+ (- 0 (* 8 id)) n))
        (r (get-r info)))
    `(,(if (< (abs n) #x80) `(,(string-append "mov____%" r ",0x8(%rbp)") (#:immediate1 ,n))
           `(,(string-append "mov____%" r ",0x32(%rbp)") (#:immediate ,n))))))

(define (x86_64:r-mem-add info v)
  (let ((r (get-r info)))
    `(,(if (< (abs v) #x80) `(,(string-append "add____$i8,(%" r ")") (#:immediate1 ,v))
           `(,(string-append "add____$i32,(%" r ")") (#:immediate ,v)))))) ;; FIXME 64bit

(define (x86_64:r-byte-mem-add info v)
  (let ((r (get-r info)))
    `((,(string-append "addb___$i8,(%" r ")") (#:immediate1 ,v)))))

(define (x86_64:r-word-mem-add info v)
  (let ((r (get-r info)))
    `((,(string-append "addw___$i8,(%" r ")") (#:immediate2 ,v)))))

(define (x86_64:local-ptr->r info n)
  (let ((r (get-r info)))
    (let ((n (- 0 (* 8 n))))
      `((,(string-append "mov____%rbp,%" r))
        ,(if (< (abs n) #x80) `(,(string-append "add____$i8,%" r) (#:immediate1 ,n))
             `(,(string-append "add____$i32,%" r)  (#:immediate ,n))))))) ;; FIXME 64bit

(define (x86_64:label->r info label)
  (let ((r (get-r info)))
    `((,(string-append "mov____$i64,%" r) (#:address8 ,label)))))

(define (x86_64:r0->r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append  "mov____%" r0 ",%" r1)))))

(define (x86_64:byte-mem->r info)
  (let ((r (get-r info)))
    `((,(string-append "movzbq_(%" r "),%" r)))))

(define (x86_64:byte-r info)
  (let* ((r (get-r info))
         (l (r->l r)))
    `((,(string-append "movzbq_%" l ",%" r)))))

(define (x86_64:byte-signed-r info)
  (let* ((r (get-r info))
         (l (r->l r)))
    `((,(string-append "movsbq_%" l ",%" r)))))

(define (x86_64:word-r info)
  (let* ((r (get-r info))
         (x (r->x r)))
    `((,(string-append "movzwq_%" x ",%" r)))))

(define (x86_64:word-signed-r info)
  (let* ((r (get-r info))
         (x (r->x r)))
    `((,(string-append "movswq_%" x ",%" r)))))

(define (x86_64:long-r info)
  (let* ((r (get-r info))
         (e (r->e r)))
    `((,(string-append "movzlq_%" e ",%" r)))))

(define (x86_64:long-signed-r info)
  (let* ((r (get-r info))
         (e (r->e r)))
    `((,(string-append "movslq_%" e ",%" r)))))

(define (x86_64:jump info label)
  `(("jmp32 " (#:offset ,label))))

(define (x86_64:jump-nz info label)
  `(("jne32 " (#:offset ,label))))

(define (x86_64:jump-z info label)
  `(("je32  " (#:offset ,label))))

(define (x86_64:jump-byte-z info label)
  `(("test___%al,%al")
    ("je32  " (#:offset ,label))))

;; signed
(define (x86_64:jump-g info label)
  `(("jg32  " (#:offset ,label))))

(define (x86_64:jump-ge info  label)
  `(("jge32 " (#:offset ,label))))

(define (x86_64:jump-l info label)
  `(("jl32  " (#:offset ,label))))

(define (x86_64:jump-le info label)
  `(("jle32 " (#:offset ,label))))

;; unsigned
(define (x86_64:jump-a info label)
  `(("ja32  " (#:offset ,label))))

(define (x86_64:jump-ae info label)
  `(("jae32 " (#:offset ,label))))

(define (x86_64:jump-b info label)
  `(("jb32  " (#:offset ,label))))

(define (x86_64:jump-be info label)
  `(("jbe32 " (#:offset ,label))))

(define (x86_64:byte-r0->r1-mem info)
  (let* ((r0 (get-r0 info))
         (r1 (get-r1 info))
         (l0 (r->l r0)))
    `((,(string-append "mov____%" l0 ",(%" r1 ")")))))

(define (x86_64:label-mem->r info label)
  (let ((r (get-r info)))
    `((,(string-append "mov____0x32,%" r) (#:address ,label)))))

(define (x86_64:word-mem->r info)
  (let ((r (get-r info)))
    `((,(string-append "movzwq_(%" r "),%" r)))))

(define (x86_64:long-mem->r info)
  (let ((r (get-r info)))
    `((,(string-append "movzlq_(%" r "),%" r)))))

(define (x86_64:mem->r info)
  (let ((r (get-r info)))
    `((,(string-append "mov____(%" r "),%" r)))))

(define (x86_64:local-add info n v)
  (let ((n (- 0 (* 8 n))))
    `(,(if (and (< (abs n) #x80)
                (< (abs v) #x80)) `("add____$i8,0x8(%rbp)" (#:immediate1 ,n) (#:immediate1 ,v))
                `("add____$i32,0x32(%rbp)" (#:immediate ,n) (#:immediate ,v)))))) ;; FIXME: 64b

(define (x86_64:label-mem-add info label v)
  `(,(if (< (abs v) #x80) `("add____$i8,0x32" (#:address ,label) (#:immediate1 ,v))
         `("add____$i32,0x32" (#:address ,label) (#:immediate ,v))))) ;; FIXME: 64b

(define (x86_64:nop info)
  '(("nop")))

(define (x86_64:swap-r0-r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "xchg___%" r0 ",%" r1)))))

;; signed
(define (x86_64:g?->r info)
  (let* ((r (get-r info))
         (l (r->l r)))
    `((,(string-append "setg___%" l))
      (,(string-append "movzbq_%" l ",%" r)))))

(define (x86_64:ge?->r info)
  (let* ((r (get-r info))
         (l (r->l r)))
    `((,(string-append "setge__%" l))
      (,(string-append "movzbq_%" l ",%" r)))))

(define (x86_64:l?->r info)
  (let* ((r (get-r info))
         (l (r->l r)))
    `((,(string-append "setl___%" l))
      (,(string-append "movzbq_%" l ",%" r)))))

(define (x86_64:le?->r info)
  (let* ((r (get-r info))
         (l (r->l r)))
    `((,(string-append "setle__%" l))
      (,(string-append "movzbq_%" l ",%" r)))))

;; unsigned
(define (x86_64:a?->r info)
  (let* ((r (get-r info))
         (l (r->l r)))
    `((,(string-append "seta___%" l))
      (,(string-append "movzbq_%" l ",%" r)))))

(define (x86_64:ae?->r info)
  (let* ((r (get-r info))
         (l (r->l r)))
    `((,(string-append "setae__%" l))
      (,(string-append "movzbq_%" l ",%" r)))))

(define (x86_64:b?->r info)
  (let* ((r (get-r info))
         (l (r->l r)))
    `((,(string-append "setb___%" l))
      (,(string-append "movzbq_%" l ",%" r)))))

(define (x86_64:be?->r info)
  (let* ((r (get-r info))
         (l (r->l r)))
    `((,(string-append "setbe__%" l))
      (,(string-append "movzbq_%" l ",%" r)))))

(define (x86_64:test-r info)
  (let ((r (get-r info)))
    `((,(string-append "test___%" r ",%" r)))))

(define (x86_64:r->label info label)
  (let ((r (get-r info)))
    `((,(string-append "mov____%" r ",0x32") (#:address ,label))))) ;; FIXME: 64 bits

(define (x86_64:r->byte-label info label)
  (let* ((r (get-r info))
         (l (r->l r)))
    `((,(string-append "movb___%" l ",0x32") (#:address ,label)))))

(define (x86_64:r->word-label info label)
  (let* ((r (get-r info))
        (x (r->x r)))
    `((,(string-append "movw___%" x ",0x32") (#:address ,label)))))

(define (x86_64:r->long-label info label)
  (let* ((r (get-r info))
        (e (r->e r)))
    `((,(string-append "movl___%" e ",0x32") (#:address ,label)))))

(define (x86_64:call-r info n)
  (let ((r (get-r info)))
    `((,(string-append "call___*%" r))
      ("add____$i8,%rsp" (#:immediate1  ,(* n 8)))))) ;; NOT AMD

(define (x86_64:r0*r1 info)
  (let ((allocated (.allocated info))
        (r0 (get-r0 info))
        (r1 (get-r1 info)))
    (if (not (member "rdx" allocated))
        `(,@(if (equal? r0 "rax") '()
                `(("push___%rax"
                   ,(string-append "mov____%" r0 ",%rax"))))
          (,(string-append "mul____%" r1))
          ,@(if (equal? r0 "rax") '()
                `((,(string-append "mov____%rax,%" r0)
                   "pop____%rax"))))
        `(("push___%rax")
          ("push___%rdi")
          ("push___%rdx")
          (,(string-append "mov____%" r1 ",%rdi"))
          (,(string-append "mov____%" r0 ",%rax"))
          (,(string-append "mul____%" r1))
          ("pop____%rdx")
          ("pop____%rdi")
          (,(string-append "mov____%rax,%" r0))
          ("pop____%rax")))))

(define (x86_64:r0<<r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "mov____%" r1 ",%rcx"))
      (,(string-append "shl____%cl,%" r0)))))

(define (x86_64:r0>>r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "mov____%" r1 ",%rcx"))
      (,(string-append "shr____%cl,%" r0)))))

(define (x86_64:r0-and-r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "and____%" r1 ",%" r0)))))

(define (x86_64:r0/r1 info signed?)
  (let ((signed? #f)              ; nobody knows, -- all advice are belong to us?
        (allocated (.allocated info))
        (r0 (get-r0 info))
        (r1 (get-r1 info)))
    (if (not (member "rdx" allocated))
        `(,@(if (equal? r0 "rax") '()
                `(("push___%rax")
                  (,(string-append "mov____%" r0 ",%rax"))))
          ,(if signed? '("cqto") '("xor____%rdx,%rdx"))
          ,(if signed? `(,(string-append "idiv___%" r1)) `(,(string-append "div___%" r1)))
          ,@(if (equal? r0 "rax") '()
                `((,(string-append "mov____%rax,%" r0))
                  ("pop____%rax"))))
        `(("push___%rax")
          ("push___%rdi")
          ("push___%rdx")
          (,(string-append "mov____%" r1 ",%rdi"))
          (,(string-append "mov____%" r0 ",%rax"))
          ,(if signed? '("cqto") '("xor____%rdx,%rdx"))
          ,(if signed? `(,(string-append "idiv___%rdi")) `(,(string-append "div___%rdi")))
          ("pop____%rdx")
          ("pop____%rdi")
          (,(string-append "mov____%rax,%" r0))
          ("pop____%rax")))))

(define (x86_64:r0%r1 info signed?)
  (let ((signed? #f)              ; nobody knows, -- all advice are belong to us?
        (allocated (.allocated info))
        (r0 (get-r0 info))
        (r1 (get-r1 info)))
    (if (not (member "rdx" allocated))
        `(,@(if (equal? r0 "rax") '()
                `(("push___%rax")
                  (,(string-append "mov____%" r0 ",%rax"))))
          ,(if signed? '("cqto") '("xor____%rdx,%rdx"))
          ,(if signed? `(,(string-append "idiv___%" r1)) `(,(string-append "div___%" r1)))
          (,(string-append "mov____%rdx,%" r0)))
        `(("push___%rax")
          ("push___%rdi")
          ("push___%rdx")
          (,(string-append "mov____%" r1 ",%rdi"))
          (,(string-append "mov____%" r0 ",%rax"))
          ,(if signed? '("cqto") '("xor____%rdx,%rdx"))
          ,(if signed? `(,(string-append "idiv___%rdi")) `(,(string-append "div___%rdi")))
          ("pop____%rdx")
          ("pop____%rdi")
          (,(string-append "mov____%rdx,%" r0))
          ("pop____%rax")))))

(define (x86_64:r+value info v)
  (let ((r (get-r info)))
    (cond ((< (abs v) #x80)
           `((,(string-append "add____$i8,%" r) (#:immediate1 ,v))))
          ((< (abs v) #x80000000)
           `((,(string-append "add____$i32,%" r) (#:immediate ,v))))
          (else
           `((,(string-append "mov____$i64,%r15") (#:immediate8 ,v))
             (,(string-append "add____%r15,%" r)))))))

(define (x86_64:r0->r1-mem info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "mov____%" r0 ",(%" r1 ")")))))

(define (x86_64:byte-r0->r1-mem info)
  (let* ((r0 (get-r0 info))
         (r1 (get-r1 info))
         (l0 (r->l r0)))
    `((,(string-append "mov____%" l0 ",(%" r1 ")")))))

(define (x86_64:word-r0->r1-mem info)
  (let* ((r0 (get-r0 info))
         (r1 (get-r1 info))
         (x0 (r->x r0)))
    `((,(string-append "mov____%" x0 ",(%" r1 ")")))))

(define (x86_64:long-r0->r1-mem info)
  (let* ((r0 (get-r0 info))
         (r1 (get-r1 info))
         (e0 (r->e r0)))
    `((,(string-append "mov____%" e0 ",(%" r1 ")")))))

(define (x86_64:r-cmp-value info v)
  (let ((r (get-r info)))
    (cond ((< (abs v) #x80)
           `((,(string-append "cmp____$i8,%" r) (#:immediate1 ,v))))
          ((and (>= v 0)
                (< v #xffffffff))
           `((,(string-append "cmp____$i32,%" r) (#:immediate ,v))))
          (else
           `(,(string-append "mov____$i64,%r15") (#:immediate8 ,v)
             ,(string-append "cmp____%r15,%" r))))))

(define (x86_64:push-register info r)
  `((,(string-append "push___%" r))))

(define (x86_64:pop-register info r)
  `((,(string-append "pop____%" r))))

(define (x86_64:return->r info)
  (let ((r (car (.allocated info))))
    (if (equal? r "rax") '()
        `((,(string-append "mov____%rax,%" r))))))

(define (x86_64:r0-or-r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "or_____%" r1 ",%" r0)))))

(define (x86_64:shl-r info n)
  (let ((r (get-r info)))
    `((,(string-append "shl____$i8,%" r) (#:immediate1 ,n)))))

(define (x86_64:r+r info)
  (let ((r (get-r info)))
    `((,(string-append "add____%" r ",%" r)))))

(define (x86_64:not-r info)
  (let ((r (get-r info)))
    `((,(string-append "not____%" r)))))

(define (x86_64:r0-xor-r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "xor____%" r1 ",%" r0)))))

(define (x86_64:r0-mem->r1-mem info)
  (let* ((registers (.registers info))
         (r0 (get-r0 info))
         (r1 (get-r1 info))
         (r2 (car registers)))
    `((,(string-append "mov____(%" r0 "),%" r2))
      (,(string-append "mov____%" r2 ",(%" r1 ")")))))

(define (x86_64:byte-r0-mem->r1-mem info)
  (let* ((registers (.registers info))
         (r0 (get-r0 info))
         (r1 (get-r1 info))
         (r2 (car registers))
         (l2 (r->l r2)))
    `((,(string-append "mov____(%" r0 "),%" l2))
      (,(string-append "mov____%" l2 ",(%" r1 ")")))))

(define (x86_64:word-r0-mem->r1-mem info)
  (let* ((registers (.registers info))
         (r0 (get-r0 info))
         (r1 (get-r1 info))
         (r2 (car registers))
         (x2 (r->x r2)))
    `((,(string-append "mov____(%" r0 "),%" x2))
      (,(string-append "mov____%" x2 ",(%" r1 ")")))))

(define (x86_64:long-r0-mem->r1-mem info)
  (let* ((registers (.registers info))
         (r0 (get-r0 info))
         (r1 (get-r1 info))
         (r2 (car registers))
         (e2 (r->e r2)))
    `((,(string-append "mov____(%" r0 "),%" e2))
      (,(string-append "mov____%" e2 ",(%" r1 ")")))))

(define (x86_64:r0+value info v)
  (let ((r0 (get-r0 info)))
    `(,(if (< (abs v) #x80) `(,(string-append "add____$i8,%" r0) (#:immediate1 ,v))
           `(,(string-append "add____$i32,%" r0) (#:immediate ,v)))))) ; FIXME: 64bit

(define (x86_64:value->r0 info v)
  (let ((r0 (get-r0 info)))
    `((,(string-append "mov____$i32,%" r0) (#:immediate ,v)))))

(define (x86_64:r-long-mem-add info v)
  (let ((r (get-r info)))
    (cond  ((< (abs v) #x80)
            `((,(string-append "addl___$i8,(%" r ")") (#:immediate1 ,v))))
           ((and (>= v 0)
                 (< v #xffffffff))
            `((,(string-append "addl___$i32,(%" r ")") (#:immediate ,v))))
           (else
            `((,(string-append "mov____$i64,%r15") (#:immediate8 ,v))
              (,(string-append "add____%r15,(%" r ")")))))))

(define (x86_64:byte-r->local+n info id n)
  (let* ((n (+ (- 0 (* 8 id)) n))
         (r (get-r info))
         (l (r->l r) ))
    `(,(if (< (abs n) #x80) `(,(string-append "mov____%" l ",0x8(%rbp)") (#:immediate1 ,n))
           `(,(string-append "mov____%" l ",0x32(%rbp)") (#:immediate ,n))))))

(define (x86_64:word-r->local+n info id n)
  (let* ((n (+ (- 0 (* 8 id)) n))
         (r (get-r info))
         (x (r->x r) ))
    `(,(if (< (abs n) #x80) `(,(string-append "mov____%" x ",0x8(%rbp)") (#:immediate1 ,n))
           `(,(string-append "mov____%" x ",0x32(%rbp)") (#:immediate ,n))))))

(define (x86_64:long-r->local+n info id n)
  (let* ((n (+ (- 0 (* 8 id)) n))
         (r (get-r info))
         (e (r->e r)))
    `(,(if (< (abs n) #x80) `(,(string-append "mov____%" e ",0x8(%rbp)") (#:immediate1 ,n))
           `(,(string-append "mov____%" e ",0x32(%rbp)") (#:immediate ,n))))))

(define (x86_64:r-and info v)
  (let ((r (get-r info)))
    (if (and (>= v 0)
             (< v #xffffffff))
        `((,(string-append "and____$i32,%" r) (#:immediate ,v)))
        `((,(string-append "mov____$i64,%r15") (#:immediate8 ,v))
          (,(string-append "and____%r15,%" r))))))

(define (x86_64:push-r0 info)
  (let ((r0 (get-r0 info)))
    `((,(string-append "push___%" r0)))))

(define (x86_64:r1->r0 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append  "mov____%" r1 ",%" r0)))))

(define (x86_64:pop-r0 info)
  (let ((r0 (get-r0 info)))
    `((,(string-append "pop____%" r0)))))

(define (x86_64:swap-r-stack info)
  (let ((r (get-r info)))
    `((,(string-append "xchg___%" r ",(%rsp)")))))

(define (x86_64:swap-r1-stack info)
  (let ((r0 (get-r0 info)))
    `((,(string-append "xchg___%" r0 ",(%rsp)")))))

(define (x86_64:r2->r0 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info))
        (allocated (.allocated info)))
    (if (> (length allocated) 2)
        (let ((r2 (cadddr allocated)))
          `((,(string-append  "mov____%" r2 ",%" r1))))
        `((,(string-append  "pop____%" r0))
          (,(string-append  "push___%" r0))))))

(define x86_64:instructions
  `(
    (a?->r . ,x86_64:a?->r)
    (ae?->r . ,x86_64:ae?->r)
    (b?->r . ,x86_64:b?->r)
    (be?->r . ,x86_64:be?->r)
    (byte-mem->r . ,x86_64:byte-mem->r)
    (byte-r . ,x86_64:byte-r)
    (byte-r->local+n . ,x86_64:byte-r->local+n)
    (byte-r0->r1-mem . ,x86_64:byte-r0->r1-mem)
    (byte-r0-mem->r1-mem . ,x86_64:byte-r0-mem->r1-mem)
    (byte-signed-r . ,x86_64:byte-signed-r)
    (call-label . ,x86_64:call-label)
    (call-r . ,x86_64:call-r)
    (function-locals . ,x86_64:function-locals)
    (function-preamble . ,x86_64:function-preamble)
    (g?->r . ,x86_64:g?->r)
    (ge?->r . ,x86_64:ge?->r)
    (jump . ,x86_64:jump)
    (jump-a . ,x86_64:jump-a)
    (jump-ae . ,x86_64:jump-ae)
    (jump-b . ,x86_64:jump-b)
    (jump-be . ,x86_64:jump-be)
    (jump-byte-z . ,x86_64:jump-byte-z)
    (jump-g . , x86_64:jump-g)
    (jump-ge . , x86_64:jump-ge)
    (jump-l . ,x86_64:jump-l)
    (jump-le . ,x86_64:jump-le)
    (jump-nz . ,x86_64:jump-nz)
    (jump-z . ,x86_64:jump-z)
    (l?->r . ,x86_64:l?->r)
    (label->arg . ,x86_64:label->arg)
    (label->r . ,x86_64:label->r)
    (label-mem->r . ,x86_64:label-mem->r)
    (label-mem-add . ,x86_64:label-mem-add)
    (le?->r . ,x86_64:le?->r)
    (local->r . ,x86_64:local->r)
    (local-add . ,x86_64:local-add)
    (local-ptr->r . ,x86_64:local-ptr->r)
    (long-mem->r . ,x86_64:long-mem->r)
    (long-r . ,x86_64:long-r)
    (long-r->local+n . ,x86_64:long-r->local+n)
    (long-r0->r1-mem . ,x86_64:long-r0->r1-mem)
    (long-r0-mem->r1-mem . ,x86_64:long-r0-mem->r1-mem)
    (long-signed-r . ,x86_64:long-signed-r)
    (mem->r . ,x86_64:mem->r)
    (nop . ,x86_64:nop)
    (not-r . ,x86_64:not-r)
    (pop-r0 . ,x86_64:pop-r0)
    (pop-register . ,x86_64:pop-register)
    (push-r0 . ,x86_64:push-r0)
    (push-register . ,x86_64:push-register)
    (quad-r0->r1-mem . ,x86_64:r0->r1-mem)
    (r+r . ,x86_64:r+r)
    (r+value . ,x86_64:r+value)
    (r->arg . ,x86_64:r->arg)
    (r->byte-label . ,x86_64:r->byte-label)
    (r->label . ,x86_64:r->label)
    (r->local . ,x86_64:r->local)
    (r->local+n . ,x86_64:r->local+n)
    (r->long-label . ,x86_64:r->long-label)
    (r->word-label . ,x86_64:r->word-label)
    (r-and . ,x86_64:r-and)
    (r-byte-mem-add . ,x86_64:r-byte-mem-add)
    (r-cmp-value . ,x86_64:r-cmp-value)
    (r-long-mem-add . ,x86_64:r-long-mem-add)
    (r-mem-add . ,x86_64:r-mem-add)
    (r-negate . ,x86_64:r-negate)
    (r-word-mem-add . ,x86_64:r-word-mem-add)
    (r-zero? . ,x86_64:r-zero?)
    (r0%r1 . ,x86_64:r0%r1)
    (r0*r1 . ,x86_64:r0*r1)
    (r0+r1 . ,x86_64:r0+r1)
    (r0+value . ,x86_64:r0+value)
    (r0->r1 . ,x86_64:r0->r1)
    (r0->r1-mem . ,x86_64:r0->r1-mem)
    (r0-and-r1 . ,x86_64:r0-and-r1)
    (r0-mem->r1-mem . ,x86_64:r0-mem->r1-mem)
    (r0-or-r1 . ,x86_64:r0-or-r1)
    (r0-r1 . ,x86_64:r0-r1)
    (r0-xor-r1 . ,x86_64:r0-xor-r1)
    (r0/r1 . ,x86_64:r0/r1)
    (r0<<r1 . ,x86_64:r0<<r1)
    (r0>>r1 . ,x86_64:r0>>r1)
    (r1->r0 . ,x86_64:r1->r0)
    (r2->r0 . ,x86_64:r2->r0)
    (ret . ,x86_64:ret)
    (return->r . ,x86_64:return->r)
    (shl-r . ,x86_64:shl-r)
    (swap-r-stack . ,x86_64:swap-r-stack)
    (swap-r0-r1 . ,x86_64:swap-r0-r1)
    (swap-r1-stack . ,x86_64:swap-r1-stack)
    (test-r . ,x86_64:test-r)
    (value->r . ,x86_64:value->r)
    (value->r0 . ,x86_64:value->r0)
    (word-mem->r . ,x86_64:word-mem->r)
    (word-r . ,x86_64:word-r)
    (word-r->local+n . ,x86_64:word-r->local+n)
    (word-r0->r1-mem . ,x86_64:word-r0->r1-mem)
    (word-r0-mem->r1-mem . ,x86_64:word-r0-mem->r1-mem)
    (word-signed-r . ,x86_64:word-signed-r)
    (xor-zf . ,x86_64:xor-zf)
    (zf->r . ,x86_64:zf->r)
    ))

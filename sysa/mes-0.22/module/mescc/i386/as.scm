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

;;; define i386 assembly

;;; Code:

(define-module (mescc i386 as)
  #:use-module (mes guile)
  #:use-module (mescc as)
  #:use-module (mescc info)
  #:export (
            i386:instructions
            ))

(define (e->x o)
  (string-drop o 1))

(define (e->l o)
  (string-append (string-drop-right (string-drop o 1) 1) "l"))


(define (i386:function-preamble . rest)
  '(("push___%ebp")
    ("mov____%esp,%ebp")))

(define (i386:function-locals . rest)
  `(("sub____$i32,%esp" (#:immediate ,(+ (* 4 1025) (* 20 4)))))) ; 4*1024 buf, 20 local vars

(define (i386:r->local info n)
  (or n (error "invalid value: i386:r->local: " n))
  (let ((r (get-r info))
        (n (- 0 (* 4 n))))
    `(,(if (< (abs n) #x80) `(,(string-append "mov____%" r ",0x8(%ebp)") (#:immediate1 ,n))
           `(,(string-append "mov____%" r ",0x32(%ebp)") (#:immediate ,n))))))

(define (i386:value->r info v)
  (let ((r (get-r info)))
    `((,(string-append "mov____$i32,%" r) (#:immediate ,v)))))

(define (i386:ret . rest)
  '(("leave")
    ("ret")))

(define (i386:r-zero? info)
  (let ((r (get-r info)))
    `((,(string-append "test___%" r "," "%" r)))))

(define (i386:local->r info n)
  (let ((r (get-r info))
        (n (- 0 (* 4 n))))
    `(,(if (< (abs n) #x80) `(,(string-append "mov____0x8(%ebp),%" r) (#:immediate1 ,n))
           `(,(string-append "mov____0x32(%ebp),%" r) (#:immediate ,n))))))

(define (i386:r0+r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "add____%" r1 ",%" r0)))))

(define (i386:call-label info label n)
  `((call32 (#:offset ,label))
    ("add____$i8,%esp" (#:immediate1 ,(* n 4)))))

(define (i386:r->arg info i)
  (let ((r (get-r info)))
    `((,(string-append "push___%" r)))))

(define (i386:label->arg info label i)
  `(("push___$i32" (#:address ,label))))

(define (i386:r-negate info)
  (let* ((r (get-r info))
         (l (e->l r)))
    `((,(string-append "sete___%" l))
      (,(string-append "movzbl_%" l ",%" r)))))

(define (i386:r0-r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "sub____%" r1 ",%" r0)))))

(define (i386:zf->r info)
  (let* ((r (get-r info))
         (l (e->l r)))
    `((,(string-append "sete___%" l))
      (,(string-append "movzbl_%" l ",%" r)))))

(define (i386:xor-zf info)
  '(("lahf")
    ("xor____$i8,%ah" (#:immediate1 #x40))
    ("sahf")))

(define (i386:r->local+n info id n)
  (let ((n (+ (- 0 (* 4 id)) n))
        (r (get-r info)))
    `(,(if (< (abs n) #x80) `(,(string-append "mov____%" r ",0x8(%ebp)") (#:immediate1 ,n))
           `(,(string-append "mov____%" r ",0x32(%ebp)") (#:immediate ,n))))))

(define (i386:r-mem-add info v)
  (let ((r (get-r info)))
    `(,(if (< (abs v) #x80) `(,(string-append "add____$i8,(%" r ")") (#:immediate1 ,v))
           `(,(string-append "add____$i32,(%" r ")") (#:immediate ,v))))))

(define (i386:r-byte-mem-add info v)
  (let ((r (get-r info)))
    `((,(string-append "addb___$i8,(%" r ")") (#:immediate1 ,v)))))

(define (i386:r-word-mem-add info v)
  (let ((r (get-r info)))
    `((,(string-append "addw___$i8,(%" r ")") (#:immediate2 ,v)))))

(define (i386:local-ptr->r info n)
  (let ((r (get-r info)))
    (let ((n (- 0 (* 4 n))))
      `((,(string-append "mov____%ebp,%" r))
        ,(if (< (abs n) #x80) `(,(string-append "add____$i8,%" r) (#:immediate1 ,n))
             `(,(string-append "add____$i32,%" r)  (#:immediate ,n)))))))

(define (i386:label->r info label)
  (let ((r (get-r info)))
    `((,(string-append "mov____$i32,%" r) (#:address ,label)))))

(define (i386:r0->r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append  "mov____%" r0 ",%" r1)))))

(define (i386:byte-mem->r info)
  (let ((r (get-r info)))
    `((,(string-append "movzbl_(%" r "),%" r)))))

(define (i386:byte-r info)
  (let* ((r (get-r info))
         (l (e->l r)))
    `((,(string-append "movzbl_%" l ",%" r)))))

(define (i386:byte-signed-r info)
  (let* ((r (get-r info))
         (l (e->l r)))
    `((,(string-append "movsbl_%" l ",%" r)))))

(define (i386:word-r info)
  (let* ((r (get-r info))
         (x (e->x r)))
    `((,(string-append "movzwl_%" x ",%" r)))))

(define (i386:word-signed-r info)
  (let* ((r (get-r info))
         (x (e->x r)))
    `((,(string-append "movswl_%" x ",%" r)))))

(define (i386:jump info label)
  `(("jmp32 " (#:offset ,label))))

(define (i386:jump-z info label)
  `(("je32  " (#:offset ,label))))

(define (i386:jump-nz info label)
  `(("jne32 " (#:offset ,label))))

(define (i386:jump-byte-z info label)
  `(("test___%al,%al")
    ("je32  " (#:offset ,label))))

;; signed
(define (i386:jump-g info label)
  `(("jg32  " (#:offset ,label))))

(define (i386:jump-ge info label)
  `(("jge32 " (#:offset ,label))))

(define (i386:jump-l info label)
  `(("jl32  " (#:offset ,label))))

(define (i386:jump-le info label)
  `(("jle32 " (#:offset ,label))))

;; unsigned
(define (i386:jump-a info label)
  `(("ja32  " (#:offset ,label))))

(define (i386:jump-ae info label)
  `(("jae32 " (#:offset ,label))))

(define (i386:jump-b info label)
  `(("jb32  " (#:offset ,label))))

(define (i386:jump-be info label)
  `(("jbe32 " (#:offset ,label))))

(define (i386:byte-r0->r1-mem info)
  (let* ((r0 (get-r0 info))
         (r1 (get-r1 info))
         (l0 (e->l r0)))
    `((,(string-append "mov____%" l0 ",(%" r1 ")")))))

(define (i386:label-mem->r info label)
  (let ((r (get-r info)))
    `((,(string-append "mov____0x32,%" r) (#:address ,label)))))

(define (i386:word-mem->r info)
  (let ((r (get-r info)))
    `((,(string-append "movzwl_(%" r "),%" r)))))

(define (i386:mem->r info)
  (let ((r (get-r info)))
    `((,(string-append "mov____(%" r "),%" r)))))

(define (i386:local-add info n v)
  (let ((n (- 0 (* 4 n))))
    `(,(if (and (< (abs n) #x80)
                (< (abs v) #x80)) `("add____$i8,0x8(%ebp)" (#:immediate1 ,n) (#:immediate1 ,v))
                `("add____$i32,0x32(%ebp)" (#:immediate ,n) (#:immediate ,v))))))

(define (i386:label-mem-add info label v)
  `(,(if (< (abs v) #x80) `("add____$i8,0x32" (#:address ,label) (#:immediate1 ,v))
         `("add____$i32,0x32" (#:address ,label) (#:immediate ,v)))))

(define (i386:nop info)
  '(("nop")))

(define (i386:swap-r0-r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "xchg___%" r0 ",%" r1)))))

;; signed
(define (i386:g?->r info)
  (let* ((r (get-r info))
         (l (e->l r)))
    `((,(string-append "setg___%" l))
      (,(string-append "movzbl_%" l ",%" r)))))

(define (i386:ge?->r info)
  (let* ((r (get-r info))
         (l (e->l r)))
    `((,(string-append "setge__%" l))
      (,(string-append "movzbl_%" l ",%" r)))))

(define (i386:l?->r info)
  (let* ((r (get-r info))
         (l (e->l r)))
    `((,(string-append "setl___%" l))
      (,(string-append "movzbl_%" l ",%" r)))))

(define (i386:le?->r info)
  (let* ((r (get-r info))
         (l (e->l r)))
    `((,(string-append "setle__%" l))
      (,(string-append "movzbl_%" l ",%" r)))))

;; unsigned
(define (i386:a?->r info)
  (let* ((r (get-r info))
         (l (e->l r)))
    `((,(string-append "seta___%" l))
      (,(string-append "movzbl_%" l ",%" r)))))

(define (i386:ae?->r info)
  (let* ((r (get-r info))
         (l (e->l r)))
    `((,(string-append "setae__%" l))
      (,(string-append "movzbl_%" l ",%" r)))))

(define (i386:b?->r info)
  (let* ((r (get-r info))
         (l (e->l r)))
    `((,(string-append "setb___%" l))
      (,(string-append "movzbl_%" l ",%" r)))))

(define (i386:be?->r info)
  (let* ((r (get-r info))
         (l (e->l r)))
    `((,(string-append "setbe__%" l))
      (,(string-append "movzbl_%" l ",%" r)))))

(define (i386:test-r info)
  (let ((r (get-r info)))
    `((,(string-append "test___%" r ",%" r)))))

(define (i386:r->label info label)
  (let ((r (get-r info)))
    `((,(string-append "mov____%" r ",0x32") (#:address ,label)))))

(define (i386:r->byte-label info label)
  (let* ((r (get-r info))
         (l (e->l r)))
    `((,(string-append "movb___%" l ",0x32") (#:address ,label)))))

(define (i386:r->word-label info label)
  (let* ((r (get-r info))
        (x (e->x r)))
    `((,(string-append "movw___%" x ",0x32") (#:address ,label)))))

(define (i386:call-r info n)
  (let ((r (get-r info)))
    `((,(string-append "call___*%" r))
      ("add____$i8,%esp" (#:immediate1  ,(* n 4))))))

(define (i386:r0*r1 info)
  (let ((allocated (.allocated info))
        (r0 (get-r0 info))
        (r1 (get-r1 info)))
    (if (not (member "edx" allocated))
        `(,@(if (equal? r0 "eax") '()
                `(("push___%eax")
                  (,(string-append "mov____%" r0 ",%eax"))))
          (,(string-append "mul____%" r1))
          ,@(if (equal? r0 "eax") '()
                `((,(string-append "mov____%eax,%" r0))
                  ("pop____%eax"))))
        `(("push___%eax")
          ("push___%ebx")
          ("push___%edx")
          (,(string-append "mov____%" r1 ",%ebx"))
          (,(string-append "mov____%" r0 ",%eax"))
          (,(string-append "mul____%" r1))
          ("pop____%edx")
          ("pop____%ebx")
          (,(string-append "mov____%eax,%" r0))
          ("pop____%eax")))))

(define (i386:r0<<r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "mov____%" r1 ",%ecx"))
      (,(string-append "shl____%cl,%" r0)))))

(define (i386:r0>>r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "mov____%" r1 ",%ecx"))
      (,(string-append "shr____%cl,%" r0)))))

(define (i386:r0-and-r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "and____%" r1 ",%" r0)))))

(define (i386:r0/r1 info signed?)
  (let ((signed? #f)              ; nobody knows, -- all advice are belong to us?
        (allocated (.allocated info))
        (r0 (get-r0 info))
        (r1 (get-r1 info)))
    (if (not (member "edx" allocated))
        `(,@(if (equal? r0 "eax") '()
                `(("push___%eax")
                  (,(string-append "mov____%" r0 ",%eax"))))
          ,(if signed? '("cltd") '("xor____%edx,%edx"))
          ,(if signed? `(,(string-append "idiv___%" r1)) `(,(string-append "div___%" r1)))
          ,@(if (equal? r0 "eax") '()
                `((,(string-append "mov____%eax,%" r0))
                  ("pop____%eax"))))
        `(("push___%eax")
          ("push___%ebx")
          ("push___%edx")
          (,(string-append "mov____%" r1 ",%ebx"))
          (,(string-append "mov____%" r0 ",%eax"))
          ,(if signed? '("cltd") '("xor____%edx,%edx"))
          ,(if signed? `(,(string-append "idiv___%ebx")) `(,(string-append "div___%ebx")))
          ("pop____%edx")
          ("pop____%ebx")
          (,(string-append "mov____%eax,%" r0))
          ("pop____%eax")))))

(define (i386:r0%r1 info signed?)
  (let ((signed? #f)              ; nobody knows, -- all advice are belong to us?
        (allocated (.allocated info))
        (r0 (get-r0 info))
        (r1 (get-r1 info)))
    (if (not (member "edx" allocated))
        `(,@(if (equal? r0 "eax") '()
                `(("push___%eax")
                  (,(string-append "mov____%" r0 ",%eax"))))
          ,(if signed? '("cltd") '("xor____%edx,%edx"))
          ,(if signed? `(,(string-append "idiv___%" r1)) `(,(string-append "div___%" r1)))
          (,(string-append "mov____%edx,%" r0)))
        `(("push___%eax")
          ("push___%ebx")
          ("push___%edx")
          (,(string-append "mov____%" r1 ",%ebx"))
          (,(string-append "mov____%" r0 ",%eax"))
          ,(if signed? '("cltd") '("xor____%edx,%edx"))
          ,(if signed? `(,(string-append "idiv___%ebx")) `(,(string-append "div___%ebx")))
          ("pop____%edx")
          ("pop____%ebx")
          (,(string-append "mov____%edx,%" r0))
          ("pop____%eax")))))

(define (i386:r+value info v)
  (let ((r (get-r info)))
    `(,(if (< (abs v) #x80) `(,(string-append "add____$i8,%" r) (#:immediate1 ,v))
           `(,(string-append "add____$i32,%" r) (#:immediate ,v))))))

(define (i386:r0->r1-mem info)
  (let ((r0 (get-r0 info))
         (r1 (get-r1 info)))
    `((,(string-append "mov____%" r0 ",(%" r1 ")")))))

(define (i386:byte-r0->r1-mem info)
  (let* ((r0 (get-r0 info))
         (r1 (get-r1 info))
         (l0 (e->l r0)))
    `((,(string-append "mov____%" l0 ",(%" r1 ")")))))

(define (i386:word-r0->r1-mem info)
  (let* ((r0 (get-r0 info))
         (r1 (get-r1 info))
         (x0 (e->x r0)))
    `((,(string-append "mov____%" x0 ",(%" r1 ")")))))

(define (i386:r-cmp-value info v)
  (let ((r (get-r info)))
    `(,(if (< (abs v) #x80) `(,(string-append "cmp____$i8,%" r) (#:immediate1 ,v))
           `(,(string-append "cmp____$i32,%" r) (#:immediate ,v))))))

(define (i386:push-register info r)
  `((,(string-append "push___%" r))))

(define (i386:pop-register info r)
  `((,(string-append "pop____%" r))))

(define (i386:return->r info)
  (let ((r (get-r info)))
    (if (equal? r "eax") '()
        `((,(string-append "mov____%eax,%" r))))))

(define (i386:r0-or-r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "or_____%" r1 ",%" r0)))))

(define (i386:shl-r info n)
  (let ((r (get-r info)))
    `((,(string-append "shl____$i8,%" r) (#:immediate1 ,n)))))

(define (i386:r+r info)
  (let ((r (get-r info)))
    `((,(string-append "add____%" r ",%" r)))))

(define (i386:not-r info)
  (let ((r (get-r info)))
    `((,(string-append "not____%" r)))))

(define (i386:r0-xor-r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "xor____%" r1 ",%" r0)))))

(define (i386:r0-mem->r1-mem info)
  (let* ((registers (.registers info))
         (r0 (get-r0 info))
         (r1 (get-r1 info))
         (r2 (car registers)))
    `((,(string-append "mov____(%" r0 "),%" r2))
      (,(string-append "mov____%" r2 ",(%" r1 ")")))))

(define (i386:byte-r0-mem->r1-mem info)
  (let* ((registers (.registers info))
         (r0 (get-r0 info))
         (r1 (get-r1 info))
         (r2 (car registers))
         (l2 (e->l r2)))
    `((,(string-append "mov____(%" r0 "),%" l2))
      (,(string-append "mov____%" l2 ",(%" r1 ")")))))

(define (i386:word-r0-mem->r1-mem info)
  (let* ((registers (.registers info))
         (r0 (get-r0 info))
         (r1 (get-r1 info))
         (r2 (car registers))
         (x2 (e->x r2)))
    `((,(string-append "mov____(%" r0 "),%" x2))
      (,(string-append "mov____%" x2 ",(%" r1 ")")))))

(define (i386:r0+value info v)
  (let ((r0 (get-r0 info)))
    `(,(if (< (abs v) #x80) `(,(string-append "add____$i8,%" r0) (#:immediate1 ,v))
           `(,(string-append "add____$i32,%" r0) (#:immediate ,v))))))

(define (i386:value->r0 info v)
  (let ((r0 (get-r0 info)))
    `((,(string-append "mov____$i32,%" r0) (#:immediate ,v)))))

(define (i386:byte-r->local+n info id n)
  (let* ((n (+ (- 0 (* 4 id)) n))
         (r (get-r info))
         (l (e->l r) ))
    `(,(if (< (abs n) #x80) `(,(string-append "mov____%" l ",0x8(%ebp)") (#:immediate1 ,n))
           `(,(string-append "mov____%" l ",0x32(%ebp)") (#:immediate ,n))))))

(define (i386:word-r->local+n info id n)
  (let* ((n (+ (- 0 (* 4 id)) n))
         (r (get-r info))
         (x (e->x r)))
    `(,(if (< (abs n) #x80) `(,(string-append "mov____%" x ",0x8(%ebp)") (#:immediate1 ,n))
           `(,(string-append "mov____%" x ",0x32(%ebp)") (#:immediate ,n))))))

(define (i386:r-and info v)
  (let ((r (get-r info)))
    `((,(string-append "and____$i32,%" r) (#:immediate ,v)))))

(define (i386:push-r0 info)
  (let ((r0 (get-r0 info)))
    `((,(string-append "push___%" r0)))))

(define (i386:r1->r0 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append  "mov____%" r1 ",%" r0)))))

(define (i386:pop-r0 info)
  (let ((r0 (get-r0 info)))
    `((,(string-append "pop____%" r0)))))

(define (i386:swap-r-stack info)
  (let ((r (get-r info)))
    `((,(string-append "xchg___%" r ",(%esp)")))))

(define (i386:swap-r1-stack info)
  (let ((r0 (get-r0 info)))
    `((,(string-append "xchg___%" r0 ",(%esp)")))))

(define (i386:r2->r0 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info))
        (allocated (.allocated info)))
    (if (> (length allocated) 2)
        (let ((r2 (cadddr allocated)))
          `((,(string-append  "mov____%" r2 ",%" r1))))
        `((,(string-append  "pop____%" r0))
          (,(string-append  "push___%" r0))))))

(define i386:instructions
  `(
    (a?->r . ,i386:a?->r)
    (ae?->r . ,i386:ae?->r)
    (b?->r . ,i386:b?->r)
    (be?->r . ,i386:be?->r)
    (byte-mem->r . ,i386:byte-mem->r)
    (byte-r . ,i386:byte-r)
    (byte-r->local+n . ,i386:byte-r->local+n)
    (byte-r0->r1-mem . ,i386:byte-r0->r1-mem)
    (byte-r0->r1-mem . ,i386:byte-r0->r1-mem)
    (byte-r0-mem->r1-mem . ,i386:byte-r0-mem->r1-mem)
    (byte-signed-r . ,i386:byte-signed-r)
    (call-label . ,i386:call-label)
    (call-r . ,i386:call-r)
    (function-locals . ,i386:function-locals)
    (function-preamble . ,i386:function-preamble)
    (g?->r . ,i386:g?->r)
    (ge?->r . ,i386:ge?->r)
    (jump . ,i386:jump)
    (jump-a . ,i386:jump-a)
    (jump-ae . ,i386:jump-ae)
    (jump-b . ,i386:jump-b)
    (jump-be . ,i386:jump-be)
    (jump-byte-z . ,i386:jump-byte-z)
    (jump-g . , i386:jump-g)
    (jump-ge . , i386:jump-ge)
    (jump-l . ,i386:jump-l)
    (jump-le . ,i386:jump-le)
    (jump-nz . ,i386:jump-nz)
    (jump-z . ,i386:jump-z)
    (l?->r . ,i386:l?->r)
    (label->arg . ,i386:label->arg)
    (label->r . ,i386:label->r)
    (label-mem->r . ,i386:label-mem->r)
    (label-mem-add . ,i386:label-mem-add)
    (le?->r . ,i386:le?->r)
    (local->r . ,i386:local->r)
    (local-add . ,i386:local-add)
    (local-ptr->r . ,i386:local-ptr->r)
    (long-r0->r1-mem . ,i386:r0->r1-mem)
    (long-r0-mem->r1-mem . ,i386:r0-mem->r1-mem)
    (mem->r . ,i386:mem->r)
    (nop . ,i386:nop)
    (not-r . ,i386:not-r)
    (pop-r0 . ,i386:pop-r0)
    (pop-register . ,i386:pop-register)
    (push-r0 . ,i386:push-r0)
    (push-register . ,i386:push-register)
    (r+r . ,i386:r+r)
    (r+value . ,i386:r+value)
    (r->arg . ,i386:r->arg)
    (r->byte-label . ,i386:r->byte-label)
    (r->label . ,i386:r->label)
    (r->local . ,i386:r->local)
    (r->local+n . ,i386:r->local+n)
    (r->word-label . ,i386:r->word-label)
    (r-and . ,i386:r-and)
    (r-byte-mem-add . ,i386:r-byte-mem-add)
    (r-cmp-value . ,i386:r-cmp-value)
    (r-mem-add . ,i386:r-mem-add)
    (r-negate . ,i386:r-negate)
    (r-word-mem-add . ,i386:r-word-mem-add)
    (r-zero? . ,i386:r-zero?)
    (r0%r1 . ,i386:r0%r1)
    (r0*r1 . ,i386:r0*r1)
    (r0+r1 . ,i386:r0+r1)
    (r0+value . ,i386:r0+value)
    (r0->r1 . ,i386:r0->r1)
    (r0->r1-mem . ,i386:r0->r1-mem)
    (r0-and-r1 . ,i386:r0-and-r1)
    (r0-mem->r1-mem . ,i386:r0-mem->r1-mem)
    (r0-or-r1 . ,i386:r0-or-r1)
    (r0-r1 . ,i386:r0-r1)
    (r0-xor-r1 . ,i386:r0-xor-r1)
    (r0/r1 . ,i386:r0/r1)
    (r0<<r1 . ,i386:r0<<r1)
    (r0>>r1 . ,i386:r0>>r1)
    (r1->r0 . ,i386:r1->r0)
    (r2->r0 . ,i386:r2->r0)
    (ret . ,i386:ret)
    (return->r . ,i386:return->r)
    (shl-r . ,i386:shl-r)
    (swap-r-stack . ,i386:swap-r-stack)
    (swap-r0-r1 . ,i386:swap-r0-r1)
    (swap-r1-stack . ,i386:swap-r1-stack)
    (test-r . ,i386:test-r)
    (value->r . ,i386:value->r)
    (value->r0 . ,i386:value->r0)
    (word-mem->r . ,i386:word-mem->r)
    (word-r . ,i386:word-r)
    (word-r->local+n . ,i386:word-r->local+n)
    (word-r0->r1-mem . ,i386:word-r0->r1-mem)
    (word-r0-mem->r1-mem . ,i386:word-r0-mem->r1-mem)
    (word-signed-r . ,i386:word-signed-r)
    (xor-zf . ,i386:xor-zf)
    (zf->r . ,i386:zf->r)
    ))

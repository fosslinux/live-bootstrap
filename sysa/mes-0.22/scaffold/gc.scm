;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright © 2016,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(define-module (guile gc))

(define (R) (reload-module (current-module)))

(define gc-size 10)
(define the-cars (make-vector gc-size '(* . *)))
(define the-cdrs (make-vector gc-size '(* . *)))
(define gc-free 0)
(define (gc-show)
  (display "\nfree:") (display gc-free) (newline)
  (display "       0       1       2       3       4       5       6       7       8       9\n")
  (display "cars:") (display the-cars) (newline)
  (display "cdrs:") (display the-cdrs) (newline))

(define (gc-show-new)
  (display "\nfree:") (display gc-free) (newline)
  (display "       0       1       2       3       4       5       6       7       8       9\n")
  (display "ncar:") (display new-cars) (newline)
  (display "ncdr:") (display new-cdrs) (newline))
(gc-show)

(define (gc-car c)
  (vector-ref the-cars (cell-index c)))

(define (gc-cdr c)
  (vector-ref the-cdrs (cell-index c)))

(define (gc-set-car! c x)
  (if (gc-pair? c) (vector-set! the-cars (cell-index c) x)))

(define (gc-set-cdr! c x)
  (if (gc-pair? c) (vector-set! the-cdrs (cell-index c) x)))

(define (gc-null? x) (eq? (car x) 'e))

(define (gc-pair? c)
  (and (pair? c) (eq? (car c) 'p)))

(define (cell-index c)
  (if (eq? (car c) 'p)
      (cdr c)))

(define (cell-value c)
  (if (member (car c) '(n s))
   (cdr c)))

(define (make-cell type . x)
  (cons type (if (pair? x) (car x) '*)))

(define (gc-alloc)
  (if (= gc-free gc-size) (gc))
  ((lambda (index)
     (set! gc-free (+ gc-free 1))
     (make-cell 'p index))
   gc-free))

(define (make-number x)
  ((lambda (cell)
     (vector-set! the-cars (cell-index cell) (make-cell 'n x))
     (gc-car cell))
   (gc-alloc)))

(define (make-symbol x)
  ((lambda (cell)
     (vector-set! the-cars (cell-index cell) (make-cell 's x))
     (gc-car cell))
   (gc-alloc)))

(define (gc-cons x y)
  ((lambda (cell)
     (vector-set! the-cars (cell-index cell) x)
     (vector-set! the-cdrs (cell-index cell) y)
     cell)
   (gc-alloc)))

(define gc-nil (make-cell 'e 0))
(define (gc-list . rest)
  (if (null? rest) gc-nil
      (gc-cons (car rest) (apply gc-list (cdr rest)))))

(define (gc-display x . cont?)
  (if (gc-pair? x) (begin (if (null? cont?) (display "("))
                          (gc-display (gc-car x))
                          (if (gc-pair? (gc-cdr x)) (display " "))
                          (if (not (gc-null? (gc-cdr x)))
                              (gc-display (gc-cdr x) #t))
                          (if (null? cont?) (display ")")))
      (if (gc-null? x) (if (not cont?) (display "()"))
          (display (cell-value x)))))

(define (gc-root)
  (filter gc-pair? (module-map (lambda (x y) (variable-ref y)) (current-module)))
  list1234)

(define new-cars (make-vector gc-size '(* . *)))
(define new-cdrs (make-vector gc-size '(* . *)))

#!
     begin-garbage-collection
       (assign free (const 0))
       (assign scan (const 0))
       (assign old (reg root))
       (assign relocate-continue
               (label reassign-root))
       (goto (label relocate-old-result-in-new))
     reassign-root
       (assign root (reg new))
       (goto (label gc-loop))

     gc-loop
       (test (op =) (reg scan) (reg free))
       (branch (label gc-flip))
       (assign old
               (op vector-ref)
               (reg new-cars)
               (reg scan))
       (assign relocate-continue
               (label update-car))
       (goto (label relocate-old-result-in-new))


     update-car
       (perform (op vector-set!)
                (reg new-cars)
                (reg scan)
                (reg new))
       (assign  old
                (op vector-ref)
                (reg new-cdrs)
                (reg scan))
       (assign  relocate-continue
                (label update-cdr))
       (goto (label relocate-old-result-in-new))
     update-cdr
       (perform (op vector-set!)
                (reg new-cdrs)
                (reg scan)
                (reg new))
       (assign  scan (op +) (reg scan) (const 1))
       (goto (label gc-loop))


     relocate-old-result-in-new
       (test (op pointer-to-pair?) (reg old))
       (branch (label pair))
       (assign new (reg old))
       (goto (reg relocate-continue))
     pair
       (assign  oldcr
                (op vector-ref)
                (reg the-cars)
                (reg old))
       (test (op broken-heart?) (reg oldcr))
       (branch  (label already-moved))
       (assign  new (reg free)) ; new location for pair
       ;; Update ‘free’ pointer.
       (assign free (op +) (reg free) (const 1))
       ;; Copy the ‘car’ and ‘cdr’ to new memory.
       (perform (op vector-set!)
                (reg new-cars)
                (reg new)
                (reg oldcr))
       (assign  oldcr
                (op vector-ref)
                (reg the-cdrs)
                (reg old))
       (perform (op vector-set!)
                (reg new-cdrs)
                (reg new)
                (reg oldcr))
       ;; Construct the broken heart.
       (perform (op vector-set!)
                (reg the-cars)
                (reg old)
                (const broken-heart))
       (perform (op vector-set!)
                (reg the-cdrs)
                (reg old)
                (reg new))
       (goto (reg relocate-continue))
     already-moved
       (assign  new
                (op vector-ref)
                (reg the-cdrs)
                (reg old))
       (goto (reg relocate-continue))

     gc-flip
       (assign temp (reg the-cdrs))
       (assign the-cdrs (reg new-cdrs))
       (assign new-cdrs (reg temp))
       (assign temp (reg the-cars))
       (assign the-cars (reg new-cars))
       (assign new-cars (reg temp))

!#

(define (gc)
  (let ((root (gc-root)))
    (display "gc root=") (display root) (newline)
    (set! gc-free 0)
    (gc-relocate root)
    (gc-loop 0)))

(define (gc-loop scan)
  (gc-show)
  (gc-show-new)
  (display "gc-loop scan=") (display scan) (newline)
  (display "gc-loop free=") (display gc-free) (newline)

  (if (eq? scan gc-free) (gc-flip)
      (let ((old (vector-ref new-cars scan)))
        (let ((new (gc-relocate old)))
          (let ((old (gc-update-car scan new)))
            (let ((new (gc-relocate old)))
              (let ((scan (gc-update-cdr scan new)))
                (gc-loop scan))))))))

(define (gc-update-car scan new) ; -> old
  (vector-set! new-cars scan new)
  (vector-ref new-cdrs scan))

(define (gc-update-cdr scan new)
  (vector-set! new-cdrs scan new)
  (+ 1 scan))

(define (broken-heart? c) (eq? (car c) '<))
(define gc-broken-heart '(< . 3))
(define (gc-relocate old) ; old -> new
  (display "gc-relocate old=") (display old) (newline)
  (display "gc-relocate old is pair?=") (display (gc-pair? old)) (newline)

  (if (not (gc-pair? old)) old
      (let ((oldcr (vector-ref the-cars (cell-index old))))
        (display "gc-relocate oldcr=") (display oldcr) (newline)
        (if (broken-heart? oldcr) old
            (let ((new (cons 'p gc-free)))
              (set! gc-free (+ 1 gc-free))
              (vector-set! new-cars (cell-index new) oldcr)
              (let ((oldcr (vector-ref the-cdrs (cell-index old))))
                (display "gc-relocate oldcr=") (display oldcr) (newline)
                (vector-set! new-cdrs (cell-index new) oldcr)
                (vector-set! the-cars (cell-index old) gc-broken-heart)
                (vector-set! the-cdrs (cell-index old) new))
              new)))))

(define (gc-flip)
  (let ((cars the-cars)
        (cdrs the-cdrs))
    (set! the-cars new-cars)
    (set! the-cdrs new-cdrs)
    (set! new-cars cars)
    (set! new-cdrs cdrs))
  (gc-show))

(define first (make-symbol 'F)) (newline)

(define one (make-number 1))
(display "\n one=") (display one) (newline)
(define two (make-number 2))
(define pair2-nil (gc-cons two gc-nil))
(display "\npair2-nil=") (display pair2-nil) (newline)
(gc-show)

(define list1-2 (gc-cons one pair2-nil))
(display "\nlist1-2=") (display list1-2) (newline)
(gc-show)

(define three (make-number 3))
(define four (make-number 4))
(define pair4-nil (gc-cons four gc-nil))
(define list3-4 (gc-cons three pair4-nil))
(define list1234 (gc-cons list1-2 list3-4))
(gc-show)

(display "\nlist1-2=") (display list1-2) (newline)
(display "\nlist3-4=") (display list3-4) (newline)
(display "lst=") (display list1234) (newline)
(gc-show)

(display "sicp-lst:") (gc-display list1234) (newline)
(gc-show)

(display "\n**** trigger gc ****\n")
(define next (gc-list (make-symbol 'N) (make-symbol 'X)))
(set! list1234 '(p . 0))
(display "sicp-lst:") (gc-display list1234) (newline)
(gc-show)
(display "next=") (display next) (newline)
(display "gc-next=") (gc-display next) (newline)
(gc-show)

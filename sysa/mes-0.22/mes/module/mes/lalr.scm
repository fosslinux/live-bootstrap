;;;
;;;; An Efficient and Portable LALR(1) Parser Generator for Scheme
;;;
;; Copyright 2014  Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;; Copyright 1993, 2010 Dominique Boucher
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(define *lalr-scm-version* "2.5.0")

(cond-expand 

 ;; -- Gambit-C
 (gambit

   (display "Gambit-C!")
   (newline)
   
  (define-macro (def-macro form . body)
    `(define-macro ,form (let () ,@body)))

  (def-macro (BITS-PER-WORD) 28)
  (def-macro (logical-or x . y) `(bitwise-ior ,x ,@y))
  (def-macro (lalr-error msg obj) `(error ,msg ,obj))

  (define pprint pretty-print)
  (define lalr-keyword? keyword?)
  (define (note-source-location lvalue tok) lvalue))
 
 ;; -- 
 (bigloo
  (define-macro (def-macro form . body)
    `(define-macro ,form (let () ,@body)))

  (define pprint (lambda (obj) (write obj) (newline)))
  (define lalr-keyword? keyword?)
  (def-macro (BITS-PER-WORD) 29)
  (def-macro (logical-or x . y) `(bit-or ,x ,@y))
  (def-macro (lalr-error msg obj) `(error "lalr-parser" ,msg ,obj))
  (define (note-source-location lvalue tok) lvalue))
 
 ;; -- Chicken
 (chicken
  
  (define-macro (def-macro form . body)
    `(define-macro ,form (let () ,@body)))

  (define pprint pretty-print)
  (define lalr-keyword? symbol?)
  (def-macro (BITS-PER-WORD) 30)
  (def-macro (logical-or x . y) `(bitwise-ior ,x ,@y))
  (def-macro (lalr-error msg obj) `(error ,msg ,obj))
  (define (note-source-location lvalue tok) lvalue))

 ;; -- STKlos
 (stklos
  (require "pp")

  (define (pprint form) (pp form :port (current-output-port)))

  (define lalr-keyword? keyword?)
  (define-macro (BITS-PER-WORD) 30)
  (define-macro (logical-or x . y) `(bit-or ,x ,@y))
  (define-macro (lalr-error msg obj) `(error 'lalr-parser ,msg ,obj))
  (define (note-source-location lvalue tok) lvalue))

 ;; -- Guile
 (guile
  (use-modules (ice-9 pretty-print))
  (use-modules (srfi srfi-9))

  (define pprint pretty-print)
  (define lalr-keyword? symbol?)
  (define-macro (BITS-PER-WORD) 30)
  (define-macro (logical-or x . y) `(logior ,x ,@y))
  (define-macro (lalr-error msg obj) `(error ,msg ,obj))
  (define (note-source-location lvalue tok)
    (if (and (supports-source-properties? lvalue)
             (not (source-property lvalue 'loc))
             (lexical-token? tok))
        (set-source-property! lvalue 'loc (lexical-token-source tok)))
    lvalue))

 ;; -- Mes
  (mes
   (define pprint display)
   (define lalr-keyword? symbol?)
   (define-macro (BITS-PER-WORD) 30)
   (define-macro (logical-or x . y) `(logior ,x ,@y))
   (define-macro (lalr-error msg obj) `(error ,msg ,obj))
   (define (note-source-location lvalue tok) lvalue)
   (define *eoi* -1))
  
 ;; -- Kawa
 (kawa
  (require 'pretty-print)
  (define (BITS-PER-WORD) 30)
  (define logical-or logior)
  (define (lalr-keyword? obj) (keyword? obj))
  (define (pprint obj) (pretty-print obj))
  (define (lalr-error msg obj) (error msg obj))
  (define (note-source-location lvalue tok) lvalue))

 ;; -- SISC
 (sisc
  (import logicops)
  (import record)
	
  (define pprint pretty-print)
  (define lalr-keyword? symbol?)
  (define-macro BITS-PER-WORD (lambda () 32))
  (define-macro logical-or (lambda (x . y) `(logor ,x ,@y)))
  (define-macro (lalr-error msg obj) `(error "~a ~S:" ,msg ,obj))
  (define (note-source-location lvalue tok) lvalue))
       
 ;; -- Gauche
 (gauche
  (use gauche.record)
  (define-macro (def-macro form . body)
    `(define-macro ,form (let () ,@body)))
  (define pprint (lambda (obj) (write obj) (newline)))
  (define lalr-keyword? symbol?)
  (def-macro (BITS-PER-WORD) 30)
  (def-macro (logical-or x . y) `(logior ,x . ,y))
  (def-macro (lalr-error msg obj) `(error "lalr-parser" ,msg ,obj))
  (define (note-source-location lvalue tok) lvalue))

 (else
  (error "Unsupported Scheme system")))


(define-record-type lexical-token
  (make-lexical-token category source value)
  lexical-token?
  (category lexical-token-category)
  (source   lexical-token-source)
  (value    lexical-token-value))


(define-record-type source-location
  (make-source-location input line column offset length)
  source-location?
  (input   source-location-input)
  (line    source-location-line)
  (column  source-location-column)
  (offset  source-location-offset)
  (length  source-location-length))



      ;; - Macros pour la gestion des vecteurs de bits

(define-macro (lalr-parser . arguments)
  (define (set-bit v b)
    (let ((x (quotient b (BITS-PER-WORD)))
	  (y (expt 2 (remainder b (BITS-PER-WORD)))))
      (vector-set! v x (logical-or (vector-ref v x) y))))

  (define (bit-union v1 v2 n)
    (do ((i 0 (+ i 1)))
	((= i n))
      (vector-set! v1 i (logical-or (vector-ref v1 i)
				    (vector-ref v2 i)))))

  ;; - Macro pour les structures de donnees

  (define (new-core)              (make-vector 4 0))
  (define (set-core-number! c n)  (vector-set! c 0 n))
  (define (set-core-acc-sym! c s) (vector-set! c 1 s))
  (define (set-core-nitems! c n)  (vector-set! c 2 n))
  (define (set-core-items! c i)   (vector-set! c 3 i))
  (define (core-number c)         (vector-ref c 0))
  (define (core-acc-sym c)        (vector-ref c 1))
  (define (core-nitems c)         (vector-ref c 2))
  (define (core-items c)          (vector-ref c 3))

  (define (new-shift)              (make-vector 3 0))
  (define (set-shift-number! c x)  (vector-set! c 0 x))
  (define (set-shift-nshifts! c x) (vector-set! c 1 x))
  (define (set-shift-shifts! c x)  (vector-set! c 2 x))
  (define (shift-number s)         (vector-ref s 0))
  (define (shift-nshifts s)        (vector-ref s 1))
  (define (shift-shifts s)         (vector-ref s 2))

  (define (new-red)                (make-vector 3 0))
  (define (set-red-number! c x)    (vector-set! c 0 x))
  (define (set-red-nreds! c x)     (vector-set! c 1 x))
  (define (set-red-rules! c x)     (vector-set! c 2 x))
  (define (red-number c)           (vector-ref c 0))
  (define (red-nreds c)            (vector-ref c 1))
  (define (red-rules c)            (vector-ref c 2))


  (define (new-set nelem)
    (make-vector nelem 0))


  (define (vector-map f v)
    (let ((vm-n (- (vector-length v) 1)))
      (let loop ((vm-low 0) (vm-high vm-n))
	(if (= vm-low vm-high)
	    (vector-set! v vm-low (f (vector-ref v vm-low) vm-low))
	    (let ((vm-middle (quotient (+ vm-low vm-high) 2)))
	      (loop vm-low vm-middle)
	      (loop (+ vm-middle 1) vm-high))))))


  ;; - Constantes
  (define STATE-TABLE-SIZE 1009)


  ;; - Tableaux 
  (define rrhs         #f)
  (define rlhs         #f)
  (define ritem        #f)
  (define nullable     #f)
  (define derives      #f)
  (define fderives     #f)
  (define firsts       #f)
  (define kernel-base  #f)
  (define kernel-end   #f)
  (define shift-symbol #f)
  (define shift-set    #f)
  (define red-set      #f)
  (define state-table  #f)
  (define acces-symbol #f)
  (define reduction-table #f)
  (define shift-table  #f)
  (define consistent   #f)
  (define lookaheads   #f)
  (define LA           #f)
  (define LAruleno     #f)
  (define lookback     #f)
  (define goto-map     #f)
  (define from-state   #f)
  (define to-state     #f)
  (define includes     #f)
  (define F            #f)
  (define action-table #f)

  ;; - Variables
  (define nitems          #f)
  (define nrules          #f)
  (define nvars           #f)
  (define nterms          #f)
  (define nsyms           #f)
  (define nstates         #f)
  (define first-state     #f)
  (define last-state      #f)
  (define final-state     #f)
  (define first-shift     #f)
  (define last-shift      #f)
  (define first-reduction #f)
  (define last-reduction  #f)
  (define nshifts         #f)
  (define maxrhs          #f)
  (define ngotos          #f)
  (define token-set-size  #f)

  (define driver-name     'lr-driver)

  (define (glr-driver?)
    (eq? driver-name 'glr-driver))
  (define (lr-driver?)
    (eq? driver-name 'lr-driver))

  (define (gen-tables! tokens gram )
    (initialize-all)
    (rewrite-grammar
     tokens
     gram
     (lambda (terms terms/prec vars gram gram/actions)
       (set! the-terminals/prec (list->vector terms/prec))
       (set! the-terminals (list->vector terms))
       (set! the-nonterminals (list->vector vars))
       (set! nterms (length terms))
       (set! nvars  (length vars))
       (set! nsyms  (+ nterms nvars))
       (let ((no-of-rules (length gram/actions))
	     (no-of-items (let loop ((l gram/actions) (count 0))
			    (if (null? l)
				count
				(loop (cdr l) (+ count (length (caar l))))))))
	 (pack-grammar no-of-rules no-of-items gram)
	 (set-derives)
	 (set-nullable)
	 (generate-states)
	 (lalr)
	 (build-tables)
	 (compact-action-table terms)
	 gram/actions))))


  (define (initialize-all)
    (set! rrhs         #f)
    (set! rlhs         #f)
    (set! ritem        #f)
    (set! nullable     #f)
    (set! derives      #f)
    (set! fderives     #f)
    (set! firsts       #f)
    (set! kernel-base  #f)
    (set! kernel-end   #f)
    (set! shift-symbol #f)
    (set! shift-set    #f)
    (set! red-set      #f)
    (set! state-table  (make-vector STATE-TABLE-SIZE '()))
    (set! acces-symbol #f)
    (set! reduction-table #f)
    (set! shift-table  #f)
    (set! consistent   #f)
    (set! lookaheads   #f)
    (set! LA           #f)
    (set! LAruleno     #f)
    (set! lookback     #f)
    (set! goto-map     #f)
    (set! from-state   #f)
    (set! to-state     #f)
    (set! includes     #f)
    (set! F            #f)
    (set! action-table #f)
    (set! nstates         #f)
    (set! first-state     #f)
    (set! last-state      #f)
    (set! final-state     #f)
    (set! first-shift     #f)
    (set! last-shift      #f)
    (set! first-reduction #f)
    (set! last-reduction  #f)
    (set! nshifts         #f)
    (set! maxrhs          #f)
    (set! ngotos          #f)
    (set! token-set-size  #f)
    (set! rule-precedences '()))


  (define (pack-grammar no-of-rules no-of-items gram)
    (set! nrules (+  no-of-rules 1))
    (set! nitems no-of-items)
    (set! rlhs (make-vector nrules #f))
    (set! rrhs (make-vector nrules #f))
    (set! ritem (make-vector (+ 1 nitems) #f))

    (let loop ((p gram) (item-no 0) (rule-no 1))
      (if (not (null? p))
	  (let ((nt (caar p)))
	    (let loop2 ((prods (cdar p)) (it-no2 item-no) (rl-no2 rule-no))
	      (if (null? prods)
		  (loop (cdr p) it-no2 rl-no2)
		  (begin
		    (vector-set! rlhs rl-no2 nt)
		    (vector-set! rrhs rl-no2 it-no2)
		    (let loop3 ((rhs (car prods)) (it-no3 it-no2))
		      (if (null? rhs)
			  (begin
			    (vector-set! ritem it-no3 (- rl-no2))
			    (loop2 (cdr prods) (+ it-no3 1) (+ rl-no2 1)))
			  (begin
			    (vector-set! ritem it-no3 (car rhs))
			    (loop3 (cdr rhs) (+ it-no3 1))))))))))))


  (define (set-derives)
    (define delts (make-vector (+ nrules 1) 0))
    (define dset  (make-vector nvars -1))

    (let loop ((i 1) (j 0))		; i = 0
      (if (< i nrules)
	  (let ((lhs (vector-ref rlhs i)))
	    (if (>= lhs 0)
		(begin
		  (vector-set! delts j (cons i (vector-ref dset lhs)))
		  (vector-set! dset lhs j)
		  (loop (+ i 1) (+ j 1)))
		(loop (+ i 1) j)))))

    (set! derives (make-vector nvars 0))

    (let loop ((i 0))
      (if (< i nvars)
	  (let ((q (let loop2 ((j (vector-ref dset i)) (s '()))
		     (if (< j 0)
			 s
			 (let ((x (vector-ref delts j)))
			   (loop2 (cdr x) (cons (car x) s)))))))
	    (vector-set! derives i q)
	    (loop (+ i 1))))))



  (define (set-nullable)
    (set! nullable (make-vector nvars #f))
    (let ((squeue (make-vector nvars #f))
	  (rcount (make-vector (+ nrules 1) 0))
	  (rsets  (make-vector nvars #f))
	  (relts  (make-vector (+ nitems nvars 1) #f)))
      (let loop ((r 0) (s2 0) (p 0))
	(let ((*r (vector-ref ritem r)))
	  (if *r
	      (if (< *r 0)
		  (let ((symbol (vector-ref rlhs (- *r))))
		    (if (and (>= symbol 0)
			     (not (vector-ref nullable symbol)))
			(begin
			  (vector-set! nullable symbol #t)
			  (vector-set! squeue s2 symbol)
			  (loop (+ r 1) (+ s2 1) p))))
		  (let loop2 ((r1 r) (any-tokens #f))
		    (let* ((symbol (vector-ref ritem r1)))
		      (if (> symbol 0)
			  (loop2 (+ r1 1) (or any-tokens (>= symbol nvars)))
			  (if (not any-tokens)
			      (let ((ruleno (- symbol)))
				(let loop3 ((r2 r) (p2 p))
				  (let ((symbol (vector-ref ritem r2)))
				    (if (> symbol 0)
					(begin
					  (vector-set! rcount ruleno
						       (+ (vector-ref rcount ruleno) 1))
					  (vector-set! relts p2
						       (cons (vector-ref rsets symbol)
							     ruleno))
					  (vector-set! rsets symbol p2)
					  (loop3 (+ r2 1) (+ p2 1)))
					(loop (+ r2 1) s2 p2)))))
			      (loop (+ r1 1) s2 p))))))
	      (let loop ((s1 0) (s3 s2))
		(if (< s1 s3)
		    (let loop2 ((p (vector-ref rsets (vector-ref squeue s1))) (s4 s3))
		      (if p
			  (let* ((x (vector-ref relts p))
				 (ruleno (cdr x))
				 (y (- (vector-ref rcount ruleno) 1)))
			    (vector-set! rcount ruleno y)
			    (if (= y 0)
				(let ((symbol (vector-ref rlhs ruleno)))
				  (if (and (>= symbol 0)
					   (not (vector-ref nullable symbol)))
				      (begin
					(vector-set! nullable symbol #t)
					(vector-set! squeue s4 symbol)
					(loop2 (car x) (+ s4 1)))
				      (loop2 (car x) s4)))
				(loop2 (car x) s4))))
		      (loop (+ s1 1) s4)))))))))



  (define (set-firsts)
    (set! firsts (make-vector nvars '()))

    ;; -- initialization
    (let loop ((i 0))
      (if (< i nvars)
	  (let loop2 ((sp (vector-ref derives i)))
	    (if (null? sp)
		(loop (+ i 1))
		(let ((sym (vector-ref ritem (vector-ref rrhs (car sp)))))
		  (if (< -1 sym nvars)
		      (vector-set! firsts i (sinsert sym (vector-ref firsts i))))
		  (loop2 (cdr sp)))))))

    ;; -- reflexive and transitive closure
    (let loop ((continue #t))
      (if continue
	  (let loop2 ((i 0) (cont #f))
	    (if (>= i nvars)
		(loop cont)
		(let* ((x (vector-ref firsts i))
		       (y (let loop3 ((l x) (z x))
			    (if (null? l)
				z
				(loop3 (cdr l)
				       (sunion (vector-ref firsts (car l)) z))))))
		  (if (equal? x y)
		      (loop2 (+ i 1) cont)
		      (begin
			(vector-set! firsts i y)
			(loop2 (+ i 1) #t))))))))

    (let loop ((i 0))
      (if (< i nvars)
	  (begin
	    (vector-set! firsts i (sinsert i (vector-ref firsts i)))
	    (loop (+ i 1))))))




  (define (set-fderives)
    (set! fderives (make-vector nvars #f))

    (set-firsts)

    (let loop ((i 0))
      (if (< i nvars)
	  (let ((x (let loop2 ((l (vector-ref firsts i)) (fd '()))
		     (if (null? l)
			 fd
			 (loop2 (cdr l)
				(sunion (vector-ref derives (car l)) fd))))))
	    (vector-set! fderives i x)
	    (loop (+ i 1))))))


  (define (closure core)
    ;; Initialization
    (define ruleset (make-vector nrules #f))

    (let loop ((csp core))
      (if (not (null? csp))
	  (let ((sym (vector-ref ritem (car csp))))
	    (if (< -1 sym nvars)
		(let loop2 ((dsp (vector-ref fderives sym)))
		  (if (not (null? dsp))
		      (begin
			(vector-set! ruleset (car dsp) #t)
			(loop2 (cdr dsp))))))
	    (loop (cdr csp)))))

    (let loop ((ruleno 1) (csp core) (itemsetv '())) ; ruleno = 0
      (if (< ruleno nrules)
	  (if (vector-ref ruleset ruleno)
	      (let ((itemno (vector-ref rrhs ruleno)))
		(let loop2 ((c csp) (itemsetv2 itemsetv))
		  (if (and (pair? c)
			   (< (car c) itemno))
		      (loop2 (cdr c) (cons (car c) itemsetv2))
		      (loop (+ ruleno 1) c (cons itemno itemsetv2)))))
	      (loop (+ ruleno 1) csp itemsetv))
	  (let loop2 ((c csp) (itemsetv2 itemsetv))
	    (if (pair? c)
		(loop2 (cdr c) (cons (car c) itemsetv2))
		(reverse itemsetv2))))))



  (define (allocate-item-sets)
    (set! kernel-base (make-vector nsyms 0))
    (set! kernel-end  (make-vector nsyms #f)))


  (define (allocate-storage)
    (allocate-item-sets)
    (set! red-set (make-vector (+ nrules 1) 0)))

					; --


  (define (initialize-states)
    (let ((p (new-core)))
      (set-core-number! p 0)
      (set-core-acc-sym! p #f)
      (set-core-nitems! p 1)
      (set-core-items! p '(0))

      (set! first-state (list p))
      (set! last-state first-state)
      (set! nstates 1)))



  (define (generate-states)
    (allocate-storage)
    (set-fderives)
    (initialize-states)
    (let loop ((this-state first-state))
      (if (pair? this-state)
	  (let* ((x (car this-state))
		 (is (closure (core-items x))))
	    (save-reductions x is)
	    (new-itemsets is)
	    (append-states)
	    (if (> nshifts 0)
		(save-shifts x))
	    (loop (cdr this-state))))))


  (define (new-itemsets itemset)
    ;; - Initialization
    (set! shift-symbol '())
    (let loop ((i 0))
      (if (< i nsyms)
	  (begin
	    (vector-set! kernel-end i '())
	    (loop (+ i 1)))))

    (let loop ((isp itemset))
      (if (pair? isp)
	  (let* ((i (car isp))
		 (sym (vector-ref ritem i)))
	    (if (>= sym 0)
		(begin
		  (set! shift-symbol (sinsert sym shift-symbol))
		  (let ((x (vector-ref kernel-end sym)))
		    (if (null? x)
			(begin
			  (vector-set! kernel-base sym (cons (+ i 1) x))
			  (vector-set! kernel-end sym (vector-ref kernel-base sym)))
			(begin
			  (set-cdr! x (list (+ i 1)))
			  (vector-set! kernel-end sym (cdr x)))))))
	    (loop (cdr isp)))))

    (set! nshifts (length shift-symbol)))



  (define (get-state sym)
    (let* ((isp  (vector-ref kernel-base sym))
	   (n    (length isp))
	   (key  (let loop ((isp1 isp) (k 0))
		   (if (null? isp1)
		       (modulo k STATE-TABLE-SIZE)
		       (loop (cdr isp1) (+ k (car isp1))))))
	   (sp   (vector-ref state-table key)))
      (if (null? sp)
	  (let ((x (new-state sym)))
	    (vector-set! state-table key (list x))
	    (core-number x))
	  (let loop ((sp1 sp))
	    (if (and (= n (core-nitems (car sp1)))
		     (let loop2 ((i1 isp) (t (core-items (car sp1))))
		       (if (and (pair? i1)
				(= (car i1)
				   (car t)))
			   (loop2 (cdr i1) (cdr t))
			   (null? i1))))
		(core-number (car sp1))
		(if (null? (cdr sp1))
		    (let ((x (new-state sym)))
		      (set-cdr! sp1 (list x))
		      (core-number x))
		    (loop (cdr sp1))))))))


  (define (new-state sym)
    (let* ((isp  (vector-ref kernel-base sym))
	   (n    (length isp))
	   (p    (new-core)))
      (set-core-number! p nstates)
      (set-core-acc-sym! p sym)
      (if (= sym nvars) (set! final-state nstates))
      (set-core-nitems! p n)
      (set-core-items! p isp)
      (set-cdr! last-state (list p))
      (set! last-state (cdr last-state))
      (set! nstates (+ nstates 1))
      p))


					; --

  (define (append-states)
    (set! shift-set
	  (let loop ((l (reverse shift-symbol)))
	    (if (null? l)
		'()
		(cons (get-state (car l)) (loop (cdr l)))))))

					; --

  (define (save-shifts core)
    (let ((p (new-shift)))
      (set-shift-number! p (core-number core))
      (set-shift-nshifts! p nshifts)
      (set-shift-shifts! p shift-set)
      (if last-shift
	  (begin
	    (set-cdr! last-shift (list p))
	    (set! last-shift (cdr last-shift)))
	  (begin
	    (set! first-shift (list p))
	    (set! last-shift first-shift)))))

  (define (save-reductions core itemset)
    (let ((rs (let loop ((l itemset))
		(if (null? l)
		    '()
		    (let ((item (vector-ref ritem (car l))))
		      (if (< item 0)
			  (cons (- item) (loop (cdr l)))
			  (loop (cdr l))))))))
      (if (pair? rs)
	  (let ((p (new-red)))
	    (set-red-number! p (core-number core))
	    (set-red-nreds!  p (length rs))
	    (set-red-rules!  p rs)
	    (if last-reduction
		(begin
		  (set-cdr! last-reduction (list p))
		  (set! last-reduction (cdr last-reduction)))
		(begin
		  (set! first-reduction (list p))
		  (set! last-reduction first-reduction)))))))


					; --

  (define (lalr)
    (set! token-set-size (+ 1 (quotient nterms (BITS-PER-WORD))))
    (set-accessing-symbol)
    (set-shift-table)
    (set-reduction-table)
    (set-max-rhs)
    (initialize-LA)
    (set-goto-map)
    (initialize-F)
    (build-relations)
    (digraph includes)
    (compute-lookaheads))

  (define (set-accessing-symbol)
    (set! acces-symbol (make-vector nstates #f))
    (let loop ((l first-state))
      (if (pair? l)
	  (let ((x (car l)))
	    (vector-set! acces-symbol (core-number x) (core-acc-sym x))
	    (loop (cdr l))))))

  (define (set-shift-table)
    (set! shift-table (make-vector nstates #f))
    (let loop ((l first-shift))
      (if (pair? l)
	  (let ((x (car l)))
	    (vector-set! shift-table (shift-number x) x)
	    (loop (cdr l))))))

  (define (set-reduction-table)
    (set! reduction-table (make-vector nstates #f))
    (let loop ((l first-reduction))
      (if (pair? l)
	  (let ((x (car l)))
	    (vector-set! reduction-table (red-number x) x)
	    (loop (cdr l))))))

  (define (set-max-rhs)
    (let loop ((p 0) (curmax 0) (length 0))
      (let ((x (vector-ref ritem p)))
	(if x
	    (if (>= x 0)
		(loop (+ p 1) curmax (+ length 1))
		(loop (+ p 1) (max curmax length) 0))
	    (set! maxrhs curmax)))))

  (define (initialize-LA)
    (define (last l)
      (if (null? (cdr l))
	  (car l)
	  (last (cdr l))))

    (set! consistent (make-vector nstates #f))
    (set! lookaheads (make-vector (+ nstates 1) #f))

    (let loop ((count 0) (i 0))
      (if (< i nstates)
	  (begin
	    (vector-set! lookaheads i count)
	    (let ((rp (vector-ref reduction-table i))
		  (sp (vector-ref shift-table i)))
	      (if (and rp
		       (or (> (red-nreds rp) 1)
			   (and sp
				(not
				 (< (vector-ref acces-symbol
						(last (shift-shifts sp)))
				    nvars)))))
		  (loop (+ count (red-nreds rp)) (+ i 1))
		  (begin
		    (vector-set! consistent i #t)
		    (loop count (+ i 1))))))

	  (begin
	    (vector-set! lookaheads nstates count)
	    (let ((c (max count 1)))
	      (set! LA (make-vector c #f))
	      (do ((j 0 (+ j 1))) ((= j c)) (vector-set! LA j (new-set token-set-size)))
	      (set! LAruleno (make-vector c -1))
	      (set! lookback (make-vector c #f)))
	    (let loop ((i 0) (np 0))
	      (if (< i nstates)
		  (if (vector-ref consistent i)
		      (loop (+ i 1) np)
		      (let ((rp (vector-ref reduction-table i)))
			(if rp
			    (let loop2 ((j (red-rules rp)) (np2 np))
			      (if (null? j)
				  (loop (+ i 1) np2)
				  (begin
				    (vector-set! LAruleno np2 (car j))
				    (loop2 (cdr j) (+ np2 1)))))
			    (loop (+ i 1) np))))))))))


  (define (set-goto-map)
    (set! goto-map (make-vector (+ nvars 1) 0))
    (let ((temp-map (make-vector (+ nvars 1) 0)))
      (let loop ((ng 0) (sp first-shift))
	(if (pair? sp)
	    (let loop2 ((i (reverse (shift-shifts (car sp)))) (ng2 ng))
	      (if (pair? i)
		  (let ((symbol (vector-ref acces-symbol (car i))))
		    (if (< symbol nvars)
			(begin
			  (vector-set! goto-map symbol
				       (+ 1 (vector-ref goto-map symbol)))
			  (loop2 (cdr i) (+ ng2 1)))
			(loop2 (cdr i) ng2)))
		  (loop ng2 (cdr sp))))

	    (let loop ((k 0) (i 0))
	      (if (< i nvars)
		  (begin
		    (vector-set! temp-map i k)
		    (loop (+ k (vector-ref goto-map i)) (+ i 1)))

		  (begin
		    (do ((i 0 (+ i 1)))
			((>= i nvars))
		      (vector-set! goto-map i (vector-ref temp-map i)))

		    (set! ngotos ng)
		    (vector-set! goto-map nvars ngotos)
		    (vector-set! temp-map nvars ngotos)
		    (set! from-state (make-vector ngotos #f))
		    (set! to-state (make-vector ngotos #f))

		    (do ((sp first-shift (cdr sp)))
			((null? sp))
		      (let* ((x (car sp))
			     (state1 (shift-number x)))
			(do ((i (shift-shifts x) (cdr i)))
			    ((null? i))
			  (let* ((state2 (car i))
				 (symbol (vector-ref acces-symbol state2)))
			    (if (< symbol nvars)
				(let ((k (vector-ref temp-map symbol)))
				  (vector-set! temp-map symbol (+ k 1))
				  (vector-set! from-state k state1)
				  (vector-set! to-state k state2))))))))))))))


  (define (map-goto state symbol)
    (let loop ((low (vector-ref goto-map symbol))
	       (high (- (vector-ref goto-map (+ symbol 1)) 1)))
      (if (> low high)
	  (begin
	    (display (list "Error in map-goto" state symbol)) (newline)
	    0)
	  (let* ((middle (quotient (+ low high) 2))
		 (s (vector-ref from-state middle)))
	    (cond
	     ((= s state)
	      middle)
	     ((< s state)
	      (loop (+ middle 1) high))
	     (else
	      (loop low (- middle 1))))))))


  (define (initialize-F)
    (set! F (make-vector ngotos #f))
    (do ((i 0 (+ i 1))) ((= i ngotos)) (vector-set! F i (new-set token-set-size)))

    (let ((reads (make-vector ngotos #f)))

      (let loop ((i 0) (rowp 0))
	(if (< i ngotos)
	    (let* ((rowf (vector-ref F rowp))
		   (stateno (vector-ref to-state i))
		   (sp (vector-ref shift-table stateno)))
	      (if sp
		  (let loop2 ((j (shift-shifts sp)) (edges '()))
		    (if (pair? j)
			(let ((symbol (vector-ref acces-symbol (car j))))
			  (if (< symbol nvars)
			      (if (vector-ref nullable symbol)
				  (loop2 (cdr j) (cons (map-goto stateno symbol)
						       edges))
				  (loop2 (cdr j) edges))
			      (begin
				(set-bit rowf (- symbol nvars))
				(loop2 (cdr j) edges))))
			(if (pair? edges)
			    (vector-set! reads i (reverse edges))))))
	      (loop (+ i 1) (+ rowp 1)))))
      (digraph reads)))

  (define (add-lookback-edge stateno ruleno gotono)
    (let ((k (vector-ref lookaheads (+ stateno 1))))
      (let loop ((found #f) (i (vector-ref lookaheads stateno)))
	(if (and (not found) (< i k))
	    (if (= (vector-ref LAruleno i) ruleno)
		(loop #t i)
		(loop found (+ i 1)))

	    (if (not found)
		(begin (display "Error in add-lookback-edge : ")
		       (display (list stateno ruleno gotono)) (newline))
		(vector-set! lookback i
			     (cons gotono (vector-ref lookback i))))))))


  (define (transpose r-arg n)
    (let ((new-end (make-vector n #f))
	  (new-R  (make-vector n #f)))
      (do ((i 0 (+ i 1)))
	  ((= i n))
	(let ((x (list 'bidon)))
	  (vector-set! new-R i x)
	  (vector-set! new-end i x)))
      (do ((i 0 (+ i 1)))
	  ((= i n))
	(let ((sp (vector-ref r-arg i)))
	  (if (pair? sp)
	      (let loop ((sp2 sp))
		(if (pair? sp2)
		    (let* ((x (car sp2))
			   (y (vector-ref new-end x)))
		      (set-cdr! y (cons i (cdr y)))
		      (vector-set! new-end x (cdr y))
		      (loop (cdr sp2))))))))
      (do ((i 0 (+ i 1)))
	  ((= i n))
	(vector-set! new-R i (cdr (vector-ref new-R i))))

      new-R))



  (define (build-relations)

    (define (get-state stateno symbol)
      (let loop ((j (shift-shifts (vector-ref shift-table stateno)))
		 (stno stateno))
	(if (null? j)
	    stno
	    (let ((st2 (car j)))
	      (if (= (vector-ref acces-symbol st2) symbol)
		  st2
		  (loop (cdr j) st2))))))

    (set! includes (make-vector ngotos #f))
    (do ((i 0 (+ i 1)))
	((= i ngotos))
      (let ((state1 (vector-ref from-state i))
	    (symbol1 (vector-ref acces-symbol (vector-ref to-state i))))
	(let loop ((rulep (vector-ref derives symbol1))
		   (edges '()))
	  (if (pair? rulep)
	      (let ((*rulep (car rulep)))
		(let loop2 ((rp (vector-ref rrhs *rulep))
			    (stateno state1)
			    (states (list state1)))
		  (let ((*rp (vector-ref ritem rp)))
		    (if (> *rp 0)
			(let ((st (get-state stateno *rp)))
			  (loop2 (+ rp 1) st (cons st states)))
			(begin

			  (if (not (vector-ref consistent stateno))
			      (add-lookback-edge stateno *rulep i))

			  (let loop2 ((done #f)
				      (stp (cdr states))
				      (rp2 (- rp 1))
				      (edgp edges))
			    (if (not done)
				(let ((*rp (vector-ref ritem rp2)))
				  (if (< -1 *rp nvars)
				      (loop2 (not (vector-ref nullable *rp))
					     (cdr stp)
					     (- rp2 1)
					     (cons (map-goto (car stp) *rp) edgp))
				      (loop2 #t stp rp2 edgp)))

				(loop (cdr rulep) edgp))))))))
	      (vector-set! includes i edges)))))
    (set! includes (transpose includes ngotos)))



  (define (compute-lookaheads)
    (let ((n (vector-ref lookaheads nstates)))
      (let loop ((i 0))
	(if (< i n)
	    (let loop2 ((sp (vector-ref lookback i)))
	      (if (pair? sp)
		  (let ((LA-i (vector-ref LA i))
			(F-j  (vector-ref F (car sp))))
		    (bit-union LA-i F-j token-set-size)
		    (loop2 (cdr sp)))
		  (loop (+ i 1))))))))



  (define (digraph relation)
    (define infinity (+ ngotos 2))
    (define INDEX (make-vector (+ ngotos 1) 0))
    (define VERTICES (make-vector (+ ngotos 1) 0))
    (define top 0)
    (define R relation)

    (define (traverse i)
      (set! top (+ 1 top))
      (vector-set! VERTICES top i)
      (let ((height top))
	(vector-set! INDEX i height)
	(let ((rp (vector-ref R i)))
	  (if (pair? rp)
	      (let loop ((rp2 rp))
		(if (pair? rp2)
		    (let ((j (car rp2)))
		      (if (= 0 (vector-ref INDEX j))
			  (traverse j))
		      (if (> (vector-ref INDEX i)
			     (vector-ref INDEX j))
			  (vector-set! INDEX i (vector-ref INDEX j)))
		      (let ((F-i (vector-ref F i))
			    (F-j (vector-ref F j)))
			(bit-union F-i F-j token-set-size))
		      (loop (cdr rp2))))))
	  (if (= (vector-ref INDEX i) height)
	      (let loop ()
		(let ((j (vector-ref VERTICES top)))
		  (set! top (- top 1))
		  (vector-set! INDEX j infinity)
		  (if (not (= i j))
		      (begin
			(bit-union (vector-ref F i)
				   (vector-ref F j)
				   token-set-size)
			(loop)))))))))

    (let loop ((i 0))
      (if (< i ngotos)
	  (begin
	    (if (and (= 0 (vector-ref INDEX i))
		     (pair? (vector-ref R i)))
		(traverse i))
	    (loop (+ i 1))))))


  ;; ----------------------------------------------------------------------
  ;; operator precedence management
  ;; ----------------------------------------------------------------------
      
  ;; a vector of precedence descriptors where each element
  ;; is of the form (terminal type precedence)
  (define the-terminals/prec #f)   ; terminal symbols with precedence 
					; the precedence is an integer >= 0
  (define (get-symbol-precedence sym)
    (caddr (vector-ref the-terminals/prec sym)))
					; the operator type is either 'none, 'left, 'right, or 'nonassoc
  (define (get-symbol-assoc sym)
    (cadr (vector-ref the-terminals/prec sym)))

  (define rule-precedences '())
  (define (add-rule-precedence! rule sym)
    (set! rule-precedences
	  (cons (cons rule sym) rule-precedences)))

  (define (get-rule-precedence ruleno)
    (cond
     ((assq ruleno rule-precedences)
      => (lambda (p)
	   (get-symbol-precedence (cdr p))))
     (else
      ;; process the rule symbols from left to right
      (let loop ((i    (vector-ref rrhs ruleno))
		 (prec 0))
	(let ((item (vector-ref ritem i)))
	  ;; end of rule
	  (if (< item 0)
	      prec
	      (let ((i1 (+ i 1)))
		(if (>= item nvars)
		    ;; it's a terminal symbol
		    (loop i1 (get-symbol-precedence (- item nvars)))
		    (loop i1 prec)))))))))

  ;; ----------------------------------------------------------------------
  ;; Build the various tables
  ;; ----------------------------------------------------------------------

  (define expected-conflicts 0)

  (define (build-tables)

    (define (resolve-conflict sym rule)
      (let ((sym-prec   (get-symbol-precedence sym))
	    (sym-assoc  (get-symbol-assoc sym))
	    (rule-prec  (get-rule-precedence rule)))
	(cond
	 ((> sym-prec rule-prec)     'shift)
	 ((< sym-prec rule-prec)     'reduce)
	 ((eq? sym-assoc 'left)      'reduce)
	 ((eq? sym-assoc 'right)     'shift)
	 (else                       'none))))

    (define conflict-messages '())

    (define (add-conflict-message . l)
      (set! conflict-messages (cons l conflict-messages)))

    (define (log-conflicts)
      (if (> (length conflict-messages) expected-conflicts)
	  (for-each
	   (lambda (message)
	     (for-each display message)
	     (newline))
	   conflict-messages)))

    ;; --- Add an action to the action table
    (define (add-action state symbol new-action)
      (let* ((state-actions (vector-ref action-table state))
	     (actions       (assv symbol state-actions)))
	(if (pair? actions)
	    (let ((current-action (cadr actions)))
	      (if (not (= new-action current-action))
		  ;; -- there is a conflict 
		  (begin
		    (if (and (<= current-action 0) (<= new-action 0))
			;; --- reduce/reduce conflict
			(begin
			  (add-conflict-message
			   "%% Reduce/Reduce conflict (reduce " (- new-action) ", reduce " (- current-action) 
			   ") on '" (get-symbol (+ symbol nvars)) "' in state " state)
			  (if (glr-driver?)
			      (set-cdr! (cdr actions) (cons new-action (cddr actions)))
			      (set-car! (cdr actions) (max current-action new-action))))
			;; --- shift/reduce conflict
			;; can we resolve the conflict using precedences?
			(case (resolve-conflict symbol (- current-action))
			  ;; -- shift
			  ((shift)   (if (glr-driver?)
					 (set-cdr! (cdr actions) (cons new-action (cddr actions)))
					 (set-car! (cdr actions) new-action)))
			  ;; -- reduce
			  ((reduce)  #f) ; well, nothing to do...
			  ;; -- signal a conflict!
			  (else      (add-conflict-message
				      "%% Shift/Reduce conflict (shift " new-action ", reduce " (- current-action)
				      ") on '" (get-symbol (+ symbol nvars)) "' in state " state)
				     (if (glr-driver?)
					 (set-cdr! (cdr actions) (cons new-action (cddr actions)))
					 (set-car! (cdr actions) new-action))))))))
          
	    (vector-set! action-table state (cons (list symbol new-action) state-actions)))
	))

    (define (add-action-for-all-terminals state action)
      (do ((i 1 (+ i 1)))
	  ((= i nterms))
	(add-action state i action)))

    (set! action-table (make-vector nstates '()))

    (do ((i 0 (+ i 1)))			; i = state
	((= i nstates))
      (let ((red (vector-ref reduction-table i)))
	(if (and red (>= (red-nreds red) 1))
	    (if (and (= (red-nreds red) 1) (vector-ref consistent i))
		(if (glr-driver?)
		    (add-action-for-all-terminals i (- (car (red-rules red))))
		    (add-action i 'default (- (car (red-rules red)))))
		(let ((k (vector-ref lookaheads (+ i 1))))
		  (let loop ((j (vector-ref lookaheads i)))
		    (if (< j k)
			(let ((rule (- (vector-ref LAruleno j)))
			      (lav  (vector-ref LA j)))
			  (let loop2 ((token 0) (x (vector-ref lav 0)) (y 1) (z 0))
			    (if (< token nterms)
				(begin
				  (let ((in-la-set? (modulo x 2)))
				    (if (= in-la-set? 1)
					(add-action i token rule)))
				  (if (= y (BITS-PER-WORD))
				      (loop2 (+ token 1)
					     (vector-ref lav (+ z 1))
					     1
					     (+ z 1))
				      (loop2 (+ token 1) (quotient x 2) (+ y 1) z)))))
			  (loop (+ j 1)))))))))

      (let ((shiftp (vector-ref shift-table i)))
	(if shiftp
	    (let loop ((k (shift-shifts shiftp)))
	      (if (pair? k)
		  (let* ((state (car k))
			 (symbol (vector-ref acces-symbol state)))
		    (if (>= symbol nvars)
			(add-action i (- symbol nvars) state))
		    (loop (cdr k))))))))

    (add-action final-state 0 'accept)
    (log-conflicts))

  (define (compact-action-table terms)
    (define (most-common-action acts)
      (let ((accums '()))
	(let loop ((l acts))
	  (if (pair? l)
	      (let* ((x (cadar l))
		     (y (assv x accums)))
		(if (and (number? x) (< x 0))
		    (if y
			(set-cdr! y (+ 1 (cdr y)))
			(set! accums (cons `(,x . 1) accums))))
		(loop (cdr l)))))

	(let loop ((l accums) (max 0) (sym #f))
	  (if (null? l)
	      sym
	      (let ((x (car l)))
		(if (> (cdr x) max)
		    (loop (cdr l) (cdr x) (car x))
		    (loop (cdr l) max sym)))))))

    (define (translate-terms acts)
      (map (lambda (act)
	     (cons (list-ref terms (car act))
		   (cdr act)))
	   acts))

    (do ((i 0 (+ i 1)))
	((= i nstates))
      (let ((acts (vector-ref action-table i)))
	(if (vector? (vector-ref reduction-table i))
	    (let ((act (most-common-action acts)))
	      (vector-set! action-table i
			   (cons `(*default* ,(if act act '*error*))
				 (translate-terms
				  (lalr-filter (lambda (x)
						 (not (and (= (length x) 2)
							   (eq? (cadr x) act))))
					       acts)))))
	    (vector-set! action-table i
			 (cons `(*default* *error*)
			       (translate-terms acts)))))))



  ;; --

  (define (rewrite-grammar tokens grammar k)

    (define eoi '*eoi*)

    (define (check-terminal term terms)
      (cond
       ((not (valid-terminal? term))
	(lalr-error "invalid terminal: " term))
       ((member term terms)
	(lalr-error "duplicate definition of terminal: " term))))

    (define (prec->type prec)
      (cdr (assq prec '((left:     . left)
			(right:    . right)
			(nonassoc: . nonassoc)))))

    (cond
     ;; --- a few error conditions
     ((not (list? tokens))
      (lalr-error "Invalid token list: " tokens))
     ((not (pair? grammar))
      (lalr-error "Grammar definition must have a non-empty list of productions" '()))

     (else
      ;; --- check the terminals
      (let loop1 ((lst            tokens)
		  (rev-terms      '())
		  (rev-terms/prec '())
		  (prec-level     0))
	(if (pair? lst)
	    (let ((term (car lst)))
	      (cond
	       ((pair? term)
		(if (and (memq (car term) '(left: right: nonassoc:))
			 (not (null? (cdr term))))
		    (let ((prec    (+ prec-level 1))
			  (optype  (prec->type (car term))))
		      (let loop-toks ((l             (cdr term))
				      (rev-terms      rev-terms)
				      (rev-terms/prec rev-terms/prec))
			(if (null? l)
			    (loop1 (cdr lst) rev-terms rev-terms/prec prec)
			    (let ((term (car l)))
			      (check-terminal term rev-terms)
			      (loop-toks
			       (cdr l)
			       (cons term rev-terms)
			       (cons (list term optype prec) rev-terms/prec))))))

		    (lalr-error "invalid operator precedence specification: " term)))

	       (else
		(check-terminal term rev-terms)
		(loop1 (cdr lst)
		       (cons term rev-terms)
		       (cons (list term 'none 0) rev-terms/prec)
		       prec-level))))

	    ;; --- check the grammar rules
	    (let loop2 ((lst grammar) (rev-nonterm-defs '()))
	      (if (pair? lst)
		  (let ((def (car lst)))
		    (if (not (pair? def))
			(lalr-error "Nonterminal definition must be a non-empty list" '())
			(let ((nonterm (car def)))
			  (cond ((not (valid-nonterminal? nonterm))
				 (lalr-error "Invalid nonterminal:" nonterm))
				((or (member nonterm rev-terms)
				     (assoc nonterm rev-nonterm-defs))
				 (lalr-error "Nonterminal previously defined:" nonterm))
				(else
				 (loop2 (cdr lst)
					(cons def rev-nonterm-defs)))))))
		  (let* ((terms        (cons eoi            (cons 'error          (reverse rev-terms))))
			 (terms/prec   (cons '(eoi none 0)  (cons '(error none 0) (reverse rev-terms/prec))))
			 (nonterm-defs (reverse rev-nonterm-defs))
			 (nonterms     (cons '*start* (map car nonterm-defs))))
		    (if (= (length nonterms) 1)
			(lalr-error "Grammar must contain at least one nonterminal" '())
			(let loop-defs ((defs      (cons `(*start* (,(cadr nonterms) ,eoi) : $1)
							 nonterm-defs))
					(ruleno    0)
					(comp-defs '()))
			  (if (pair? defs)
			      (let* ((nonterm-def  (car defs))
				     (compiled-def (rewrite-nonterm-def
						    nonterm-def
						    ruleno
						    terms nonterms)))
				(loop-defs (cdr defs)
					   (+ ruleno (length compiled-def))
					   (cons compiled-def comp-defs)))

			      (let ((compiled-nonterm-defs (reverse comp-defs)))
				(k terms
				   terms/prec
				   nonterms
				   (map (lambda (x) (cons (caaar x) (map cdar x)))
					compiled-nonterm-defs)
				   (apply append compiled-nonterm-defs))))))))))))))


  (define (rewrite-nonterm-def nonterm-def ruleno terms nonterms)

    (define No-NT (length nonterms))

    (define (encode x)
      (let ((PosInNT (pos-in-list x nonterms)))
	(if PosInNT
	    PosInNT
	    (let ((PosInT (pos-in-list x terms)))
	      (if PosInT
		  (+ No-NT PosInT)
		  (lalr-error "undefined symbol : " x))))))

    (define (process-prec-directive rhs ruleno)
      (let loop ((l rhs))
	(if (null? l)
	    '()
	    (let ((first (car l))
		  (rest  (cdr l)))
	      (cond
	       ((or (member first terms) (member first nonterms))
		(cons first (loop rest)))
	       ((and (pair? first)
		     (eq? (car first) 'prec:))
		(if (and (pair? (cdr first))
			 (null? (cddr first))
			 (member (cadr first) terms))
		    (if (null? rest)
			(begin
			  (add-rule-precedence! ruleno (pos-in-list (cadr first) terms))
			  (loop rest))
			(lalr-error "prec: directive should be at end of rule: " rhs))
		    (lalr-error "Invalid prec: directive: " first)))
	       (else
		(lalr-error "Invalid terminal or nonterminal: " first)))))))

    (define (check-error-production rhs)
      (let loop ((rhs rhs))
	(if (pair? rhs)
	    (begin
	      (if (and (eq? (car rhs) 'error)
		       (or (null? (cdr rhs))
			   (not (member (cadr rhs) terms))
			   (not (null? (cddr rhs)))))
		  (lalr-error "Invalid 'error' production. A single terminal symbol must follow the 'error' token.:" rhs))
	      (loop (cdr rhs))))))


    (if (not (pair? (cdr nonterm-def)))
	(lalr-error "At least one production needed for nonterminal:" (car nonterm-def))
	(let ((name (symbol->string (car nonterm-def))))
	  (let loop1 ((lst (cdr nonterm-def))
		      (i 1)
		      (rev-productions-and-actions '()))
	    (if (not (pair? lst))
		(reverse rev-productions-and-actions)
		(let* ((rhs  (process-prec-directive (car lst) (+ ruleno i -1)))
		       (rest (cdr lst))
		       (prod (map encode (cons (car nonterm-def) rhs))))
		  ;; -- check for undefined tokens
		  (for-each (lambda (x)
			      (if (not (or (member x terms) (member x nonterms)))
				  (lalr-error "Invalid terminal or nonterminal:" x)))
			    rhs)
		  ;; -- check 'error' productions
		  (check-error-production rhs)

		  (if (and (pair? rest)
			   (eq? (car rest) ':)
			   (pair? (cdr rest)))
		      (loop1 (cddr rest)
			     (+ i 1)
			     (cons (cons prod (cadr rest))
				   rev-productions-and-actions))
		      (let* ((rhs-length (length rhs))
			     (action
			      (cons 'vector
				    (cons (list 'quote (string->symbol
							(string-append
							 name
							 "-"
							 (number->string i))))
					  (let loop-j ((j 1))
					    (if (> j rhs-length)
						'()
						(cons (string->symbol
						       (string-append
							"$"
							(number->string j)))
						      (loop-j (+ j 1)))))))))
			(loop1 rest
			       (+ i 1)
			       (cons (cons prod action)
				     rev-productions-and-actions))))))))))

  (define (valid-nonterminal? x)
    (symbol? x))

  (define (valid-terminal? x)
    (symbol? x))			; DB 

  ;; ----------------------------------------------------------------------
  ;; Miscellaneous
  ;; ----------------------------------------------------------------------
  (define (pos-in-list x lst)
    (let loop ((lst lst) (i 0))
      (cond ((not (pair? lst))    #f)
	    ((equal? (car lst) x) i)
	    (else                 (loop (cdr lst) (+ i 1))))))

  (define (sunion lst1 lst2)		; union of sorted lists
    (let loop ((L1 lst1)
	       (L2 lst2))
      (cond ((null? L1)    L2)
	    ((null? L2)    L1)
	    (else
	     (let ((x (car L1)) (y (car L2)))
	       (cond
		((> x y)
		 (cons y (loop L1 (cdr L2))))
		((< x y)
		 (cons x (loop (cdr L1) L2)))
		(else
		 (loop (cdr L1) L2))
		))))))

  (define (sinsert elem lst)
    (let loop ((l1 lst))
      (if (null? l1)
	  (cons elem l1)
	  (let ((x (car l1)))
	    (cond ((< elem x)
		   (cons elem l1))
		  ((> elem x)
		   (cons x (loop (cdr l1))))
		  (else
		   l1))))))

  (define (lalr-filter p lst)
    (let loop ((l lst))
      (if (null? l)
	  '()
	  (let ((x (car l)) (y (cdr l)))
	    (if (p x)
		(cons x (loop y))
		(loop y))))))
      
  ;; ----------------------------------------------------------------------
  ;; Debugging tools ...
  ;; ----------------------------------------------------------------------
  (define the-terminals #f)		; names of terminal symbols
  (define the-nonterminals #f)		; non-terminals

  (define (print-item item-no)
    (let loop ((i item-no))
      (let ((v (vector-ref ritem i)))
	(if (>= v 0)
	    (loop (+ i 1))
	    (let* ((rlno    (- v))
		   (nt      (vector-ref rlhs rlno)))
	      (display (vector-ref the-nonterminals nt)) (display " --> ")
	      (let loop ((i (vector-ref rrhs rlno)))
		(let ((v (vector-ref ritem i)))
		  (if (= i item-no)
		      (display ". "))
		  (if (>= v 0)
		      (begin
			(display (get-symbol v))
			(display " ")
			(loop (+ i 1)))
		      (begin
			(display "   (rule ")
			(display (- v))
			(display ")")
			(newline))))))))))

  (define (get-symbol n)
    (if (>= n nvars)
	(vector-ref the-terminals (- n nvars))
	(vector-ref the-nonterminals n)))


  (define (print-states)
    (define (print-action act)
      (cond
       ((eq? act '*error*)
	(display " : Error"))
       ((eq? act 'accept)
	(display " : Accept input"))
       ((< act 0)
	(display " : reduce using rule ")
	(display (- act)))
       (else
	(display " : shift and goto state ")
	(display act)))
      (newline)
      #t)

    (define (print-actions acts)
      (let loop ((l acts))
	(if (null? l)
	    #t
	    (let ((sym (caar l))
		  (act (cadar l)))
	      (display "   ")
	      (cond
	       ((eq? sym 'default)
		(display "default action"))
	       (else
		(if (number? sym)
		    (display (get-symbol (+ sym nvars)))
		    (display sym))))
	      (print-action act)
	      (loop (cdr l))))))

    (if (not action-table)
	(begin
	  (display "No generated parser available!")
	  (newline)
	  #f)
	(begin
	  (display "State table") (newline)
	  (display "-----------") (newline) (newline)

	  (let loop ((l first-state))
	    (if (null? l)
		#t
		(let* ((core  (car l))
		       (i     (core-number core))
		       (items (core-items core))
		       (actions (vector-ref action-table i)))
		  (display "state ") (display i) (newline)
		  (newline)
		  (for-each (lambda (x) (display "   ") (print-item x))
			    items)
		  (newline)
		  (print-actions actions)
		  (newline)
		  (loop (cdr l))))))))



  ;; ----------------------------------------------------------------------
      
  (define build-goto-table
    (lambda ()
      `(vector
	,@(map
	   (lambda (shifts)
	     (list 'quote
		   (if shifts
		       (let loop ((l (shift-shifts shifts)))
			 (if (null? l)
			     '()
			     (let* ((state  (car l))
				    (symbol (vector-ref acces-symbol state)))
			       (if (< symbol nvars)
				   (cons `(,symbol . ,state)
					 (loop (cdr l)))
				   (loop (cdr l))))))
		       '())))
	   (vector->list shift-table)))))


  (define build-reduction-table
    (lambda (gram/actions)
      `(vector
	'()
	,@(map
	   (lambda (p)
	     (let ((act (cdr p)))
	       `(lambda ,(if (eq? driver-name 'lr-driver)
			     '(___stack ___sp ___goto-table ___push yypushback)
			     '(___sp ___goto-table ___push))
		  ,(let* ((nt (caar p)) (rhs (cdar p)) (n (length rhs)))
		     `(let* (,@(if act
				   (let loop ((i 1) (l rhs))
				     (if (pair? l)
					 (let ((rest (cdr l))
                                               (ns (number->string (+ (- n i) 1))))
                                           (cons
                                            `(tok ,(if (eq? driver-name 'lr-driver)
                                                       `(vector-ref ___stack (- ___sp ,(- (* i 2) 1)))
                                                       `(list-ref ___sp ,(+ (* (- i 1) 2) 1))))
                                            (cons
                                             `(,(string->symbol (string-append "$" ns))
                                               (if (lexical-token? tok) (lexical-token-value tok) tok))
                                             (cons
                                              `(,(string->symbol (string-append "@" ns))
                                                (if (lexical-token? tok) (lexical-token-source tok) tok))
                                              (loop (+ i 1) rest)))))
					 '()))
				   '()))
			,(if (= nt 0)
			     '$1
			     `(___push ,n ,nt ,(cdr p) ,@(if (eq? driver-name 'lr-driver) '() '(___sp)) 
                                       ,(if (eq? driver-name 'lr-driver)
                                            `(vector-ref ___stack (- ___sp ,(length rhs)))
                                            `(list-ref ___sp ,(length rhs))))))))))

	   gram/actions))))



  ;; Options

  (define *valid-options*
    (list
     (cons 'out-table:
	   (lambda (option)
	     (and (list? option)
		  (= (length option) 2)
		  (string? (cadr option)))))
     (cons 'output:
	   (lambda (option)
	     (and (list? option)
		  (= (length option) 3)
		  (symbol? (cadr option))
		  (string? (caddr option)))))
     (cons 'expect:
	   (lambda (option)
	     (and (list? option)
		  (= (length option) 2)
		  (integer? (cadr option))
		  (>= (cadr option) 0))))

     (cons 'driver:
	   (lambda (option)
	     (and (list? option)
		  (= (length option) 2)
		  (symbol? (cadr option))
		  (memq (cadr option) '(lr glr)))))))


  (define (validate-options options)
    (for-each
     (lambda (option)
       (let ((p (assoc (car option) *valid-options*)))
	 (if (or (not p)
		 (not ((cdr p) option)))
	     (lalr-error "Invalid option:" option))))
     options))


  (define (output-parser! options code)
    (let ((option (assq 'output: options)))
      (if option
	  (let ((parser-name (cadr option))
		(file-name   (caddr option)))
	    (with-output-to-file file-name
	      (lambda ()
		(pprint `(define ,parser-name ,code))
		(newline)))))))


  (define (output-table! options)
    (let ((option (assq 'out-table: options)))
      (if option
	  (let ((file-name (cadr option)))
	    (with-output-to-file file-name print-states)))))


  (define (set-expected-conflicts! options)
    (let ((option (assq 'expect: options)))
      (set! expected-conflicts (if option (cadr option) 0))))

  (define (set-driver-name! options)
    (let ((option (assq 'driver: options)))
      (if option
	  (let ((driver-type (cadr option)))
	    (set! driver-name (if (eq? driver-type 'glr) 'glr-driver 'lr-driver))))))


  ;; -- arguments

  (define (extract-arguments lst proc)
    (let loop ((options '())
	       (tokens  '())
	       (rules   '())
	       (lst     lst))
      (if (pair? lst)
	  (let ((p (car lst)))
	    (cond
	     ((and (pair? p)
		   (lalr-keyword? (car p))
		   (assq (car p) *valid-options*))
	      (loop (cons p options) tokens rules (cdr lst)))
	     (else
	      (proc options p (cdr lst)))))
	  (lalr-error "Malformed lalr-parser form" lst))))


  (define (build-driver options tokens rules)
    (validate-options options)
    (set-expected-conflicts! options)
    (set-driver-name! options)
    (let* ((gram/actions (gen-tables! tokens rules))
	   (code         `(,driver-name ',action-table ,(build-goto-table) ,(build-reduction-table gram/actions))))
    
      (output-table! options)
      (output-parser! options code)
      code))

  (extract-arguments arguments build-driver))
   


;;;
;;;; --
;;;; Implementation of the lr-driver
;;;


(cond-expand
 (gambit
  (declare
   (standard-bindings)
   (fixnum)
   (block)
   (not safe)))
 (chicken
  (declare
   (uses extras)
   (usual-integrations)
   (fixnum)
   (not safe)))
 (guile)
 (else))


;;;
;;;; Source location utilities
;;;


;; This function assumes that src-location-1 and src-location-2 are source-locations
;; Returns #f if they are not locations for the same input 
(define (combine-locations src-location-1 src-location-2)
  (let ((offset-1 (source-location-offset src-location-1))
        (offset-2 (source-location-offset src-location-2))
        (length-1 (source-location-length src-location-1))
        (length-2 (source-location-length src-location-2)))

    (cond ((not (equal? (source-location-input src-location-1)
                        (source-location-input src-location-2)))
           #f)
          ((or (not (number? offset-1)) (not (number? offset-2))
               (not (number? length-1)) (not (number? length-2))
               (< offset-1 0) (< offset-2 0)
               (< length-1 0) (< length-2 0))
           (make-source-location (source-location-input src-location-1)
                                 (source-location-line src-location-1)
                                 (source-location-column src-location-1)
                                 -1 -1))
          ((<= offset-1 offset-2)
           (make-source-location (source-location-input src-location-1)
                                 (source-location-line src-location-1)
                                 (source-location-column src-location-1)
                                 offset-1
                                 (- (+ offset-2 length-2) offset-1)))
          (else
           (make-source-location (source-location-input src-location-1)
                                 (source-location-line src-location-1)
                                 (source-location-column src-location-1)
                                 offset-2
                                 (- (+ offset-1 length-1) offset-2))))))


;;;
;;;;  LR-driver
;;;


(define *max-stack-size* 500)

(define (lr-driver action-table goto-table reduction-table)
  (define ___atable action-table)
  (define ___gtable goto-table)
  (define ___rtable reduction-table)

  (define ___lexerp #f)
  (define ___errorp #f)
  
  (define ___stack  #f)
  (define ___sp     0)
  
  (define ___curr-input #f)
  (define ___reuse-input #f)
  
  (define ___input #f)
  (define (___consume)
    (set! ___input (if ___reuse-input ___curr-input (___lexerp)))
    (set! ___reuse-input #f)
    (set! ___curr-input ___input))
  
  (define (___pushback)
    (set! ___reuse-input #t))
  
  (define (___initstack)
    (set! ___stack (make-vector *max-stack-size* 0))
    (set! ___sp 0))
  
  (define (___growstack)
    (let ((new-stack (make-vector (* 2 (vector-length ___stack)) 0)))
      (let loop ((i (- (vector-length ___stack) 1)))
        (if (>= i 0)
	    (begin
	      (vector-set! new-stack i (vector-ref ___stack i))
	      (loop (- i 1)))))
      (set! ___stack new-stack)))
  
  (define (___checkstack)
    (if (>= ___sp (vector-length ___stack))
        (___growstack)))
  
  (define (___push delta new-category lvalue tok)
    (set! ___sp (- ___sp (* delta 2)))
    (let* ((state     (vector-ref ___stack ___sp))
           (new-state (cdr (assoc new-category (vector-ref ___gtable state)))))
      (set! ___sp (+ ___sp 2))
      (___checkstack)
      (vector-set! ___stack ___sp new-state)
      (vector-set! ___stack (- ___sp 1) (note-source-location lvalue tok))))
  
  (define (___reduce st)
    ((vector-ref ___rtable st) ___stack ___sp ___gtable ___push ___pushback))
  
  (define (___shift token attribute)
    (set! ___sp (+ ___sp 2))
    (___checkstack)
    (vector-set! ___stack (- ___sp 1) attribute)
    (vector-set! ___stack ___sp token))
  
  (define (___action x l)
    (let ((y (assoc x l)))
      (if y (cadr y) (cadar l))))
  
  (define (___recover tok)
    (let find-state ((sp ___sp))
      (if (< sp 0)
          (set! ___sp sp)
          (let* ((state (vector-ref ___stack sp))
                 (act   (assoc 'error (vector-ref ___atable state))))
            (if act
                (begin
                  (set! ___sp sp)
                  (___sync (cadr act) tok))
                (find-state (- sp 2)))))))
  
  (define (___sync state tok)
    (let ((sync-set (map car (cdr (vector-ref ___atable state)))))
      (set! ___sp (+ ___sp 4))
      (___checkstack)
      (vector-set! ___stack (- ___sp 3) #f)
      (vector-set! ___stack (- ___sp 2) state)
      (let skip ()
        (let ((i (___category ___input)))
          (if (eq? i '*eoi*)
              (set! ___sp -1)
              (if (memq i sync-set)
                  (let ((act (assoc i (vector-ref ___atable state))))
                    (vector-set! ___stack (- ___sp 1) #f)
                    (vector-set! ___stack ___sp (cadr act)))
                  (begin
                    (___consume)
                    (skip))))))))
  
  (define (___category tok)
    (if (lexical-token? tok)
        (lexical-token-category tok)
        tok))

  (define (___run)
    (let loop ()
      (if ___input
          (let* ((state (vector-ref ___stack ___sp))
                 (i     (___category ___input))
                 (act   (___action i (vector-ref ___atable state))))
            
            (cond ((not (symbol? i))
                   (___errorp "Syntax error: invalid token: " ___input)
                   #f)
             
                  ;; Input succesfully parsed
                  ((eq? act 'accept)
                   (vector-ref ___stack 1))
                  
                  ;; Syntax error in input
                  ((eq? act '*error*)
                   (if (eq? i '*eoi*)
                       (begin
                         (___errorp "Syntax error: unexpected end of input")
                         #f)
                       (begin
                         (___errorp "Syntax error: unexpected token : " ___input)
                         (___recover i)
                         (if (>= ___sp 0)
                             (set! ___input #f)
                             (begin
                               (set! ___sp 0)
                               (set! ___input '*eoi*)))
                         (loop))))
             
                  ;; Shift current token on top of the stack
                  ((>= act 0)
                   (___shift act ___input)
                   (set! ___input (if (eq? i '*eoi*) '*eoi* #f))
                   (loop))
             
                  ;; Reduce by rule (- act)
                  (else
                   (___reduce (- act))
                   (loop))))
          
          ;; no lookahead, so check if there is a default action
          ;; that does not require the lookahead
          (let* ((state  (vector-ref ___stack ___sp))
                 (acts   (vector-ref ___atable state))
                 (defact (if (pair? acts) (cadar acts) #f)))
            (if (and (= 1 (length acts)) (< defact 0))
                (___reduce (- defact))
                (___consume))
            (loop)))))
  

  (lambda (lexerp errorp)
    (set! ___errorp errorp)
    (set! ___lexerp lexerp)
    (___initstack)
    (___run)))


;;;
;;;;  Simple-minded GLR-driver
;;;


(define (glr-driver action-table goto-table reduction-table)
  (define ___atable action-table)
  (define ___gtable goto-table)
  (define ___rtable reduction-table)

  (define ___lexerp #f)
  (define ___errorp #f)
  
  ;; -- Input handling 
  
  (define *input* #f)
  (define (initialize-lexer lexer)
    (set! ___lexerp lexer)
    (set! *input* #f))
  (define (consume)
    (set! *input* (___lexerp)))
  
  (define (token-category tok)
    (if (lexical-token? tok)
        (lexical-token-category tok)
        tok))

  (define (token-attribute tok)
    (if (lexical-token? tok)
        (lexical-token-value tok)
        tok))

  ;; -- Processes (stacks) handling
  
  (define *processes* '())
  
  (define (initialize-processes)
    (set! *processes* '()))
  (define (add-process process)
    (set! *processes* (cons process *processes*)))
  (define (get-processes)
    (reverse *processes*))
  
  (define (for-all-processes proc)
    (let ((processes (get-processes)))
      (initialize-processes)
      (for-each proc processes)))
  
  ;; -- parses
  (define *parses* '())
  (define (get-parses)
    *parses*)
  (define (initialize-parses)
    (set! *parses* '()))
  (define (add-parse parse)
    (set! *parses* (cons parse *parses*)))
    

  (define (push delta new-category lvalue stack tok)
    (let* ((stack     (drop stack (* delta 2)))
           (state     (car stack))
           (new-state (cdr (assv new-category (vector-ref ___gtable state)))))
        (cons new-state (cons (note-source-location lvalue tok) stack))))
  
  (define (reduce state stack)
    ((vector-ref ___rtable state) stack ___gtable push))
  
  (define (shift state symbol stack)
    (cons state (cons symbol stack)))
  
  (define (get-actions token action-list)
    (let ((pair (assoc token action-list)))
      (if pair 
          (cdr pair)
          (cdar action-list)))) ;; get the default action
  

  (define (run)
    (let loop-tokens ()
      (consume)
      (let ((symbol (token-category *input*)))
        (for-all-processes
         (lambda (process)
           (let loop ((stacks (list process)) (active-stacks '()))
             (cond ((pair? stacks)
                    (let* ((stack   (car stacks))
                           (state   (car stack)))
                      (let actions-loop ((actions      (get-actions symbol (vector-ref ___atable state)))
                                         (active-stacks active-stacks))
                        (if (pair? actions)
                            (let ((action        (car actions))
                                  (other-actions (cdr actions)))
                              (cond ((eq? action '*error*)
                                     (actions-loop other-actions active-stacks))
                                    ((eq? action 'accept)
                                     (add-parse (car (take-right stack 2)))
                                     (actions-loop other-actions active-stacks))
                                    ((>= action 0)
                                     (let ((new-stack (shift action *input* stack)))
                                       (add-process new-stack))
                                     (actions-loop other-actions active-stacks))
                                    (else
                                     (let ((new-stack (reduce (- action) stack)))
                                      (actions-loop other-actions (cons new-stack active-stacks))))))
                            (loop (cdr stacks) active-stacks)))))
                   ((pair? active-stacks)
                    (loop (reverse active-stacks) '())))))))
      (if (pair? (get-processes))
          (loop-tokens))))

  
  (lambda (lexerp errorp)
    (set! ___errorp errorp)
    (initialize-lexer lexerp)
    (initialize-processes)
    (initialize-parses)
    (add-process '(0))
    (run)
    (get-parses)))


(define (drop l n)
  (cond ((and (> n 0) (pair? l))
	 (drop (cdr l) (- n 1)))
	(else
	 l)))

(define (take-right l n)
  (drop l (- (length l) n)))

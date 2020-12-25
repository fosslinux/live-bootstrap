;;; nyacc/lex.scm

;; Copyright (C) 2015-2019 - Matthew R.Wette
;; 
;; This library is free software; you can redistribute it and/or modify it
;; under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This library is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this library; if not, see <http://www.gnu.org/licenses/>

;;; Description:

;; A module providing procedures for constructing lexical analyzers.

;; '$fixed '$float '$string '$ident '$chlit '$chlit/L '$chlit/u '$chlit/U

;; todo: change lexer to return @code{cons-source} instead of @code{cons}
;; todo: to be fully compliant, C readers need to deal with \ at end of line

;; todo: figure out what readers return atoms and which pairs
;; tokens: read-c-ident 
;; pairs: num-reader read-c-num read-c-string
;; issue: if returning pairs we need this for hashed parsers:
;;    (define (assc-$ pair) (cons (assq-ref symbols (car pair)) (cdr pair)))
;; read-comm changed to (read-comm ch bol) where bol is begin-of-line cond
;; 
;; read-c-ident 

;;; Code:

(define-module (nyacc lex)
  #:export (make-lexer-generator
	    make-ident-reader
	    make-comm-reader
	    make-string-reader
	    make-chseq-reader
	    make-num-reader
	    eval-reader
 	    read-c-ident read-c$-ident
 	    read-c-comm
	    read-c-string
	    read-c-chlit
	    read-c-num
	    read-oct read-hex
	    like-c-ident? like-c$-ident?
	    c-escape
	    cnumstr->scm
	    filter-mt remove-mt map-mt make-ident-like-p
	    c:ws c:if c:ir)
  #:use-module ((srfi srfi-1) #:select (remove append-reverse)))

(define (sf fmt . args) (apply simple-format #t fmt args))
  
;; @section Constructing Lexical Analyzers
;; The @code{lex} module provides a set of procedures to build lexical
;; analyzers.  The approach is to first build a set of @defn{readers} for 
;; MORE TO COME
;;
;; Readers are procecures that take one character (presumably from the
;; current-input-port) and determine try to make a match.   If a match is
;; made something is returned, with any lookaheads pushed back into the
;; input port.  If no match is made @code{#f} is returned and the input
;; argument is still the character to work on.
;;
;; Here are the procedures used:
;; @table @code

(define digit "0123456789")
(define ucase "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define lcase "abcdefghijklmnopqrstuvwxyz")

;; C lexemes are popular so include those.
;;(define c:ws (list->char-set '(#\space #\tab #\newline #\return )))
(define c:ws char-set:whitespace)
(define c:if (let ((cs (char-set #\_)))	; ident, first char
	       (string->char-set! ucase cs)
	       (string->char-set! lcase cs)))
(define c:ir (string->char-set digit c:if)) ; ident, rest chars
(define c:nx (string->char-set "eEdD"))	    ; number exponent
(define c:hx (string->char-set "abcdefABCDEF"))
(define c:sx (string->char-set "lLuU")) ; fixed suffix
(define c:fx (string->char-set "fFlL")) ; float suffix
(define c:px (string->char-set "rRhHkKlLuU")) ; fixed-point suffix
(define c:bx (string->char-set "pP"))	; binary float suffix
(define c:cx (string->char-set "LuU"))	; char prefix

(define lxlsr reverse-list->string)

;; @deffn {Procedure} eval-reader reader string => result
;; For test and debug, this procedure will evaluate a reader on a string.
;; A reader is a procedure that accepts a single character argument intended
;; to match a specific character sequence.  A reader will read more characters
;; by evaluating @code{read-char} until it matches or fails.  If it fails, it
;; will pushback all characters read via @code{read-char} and return @code{#f}.
;; If it succeeds the input pointer will be at the position following the
;; last matched character.
;; @end deffn
(define (eval-reader reader string)
  (with-input-from-string string
    (lambda () (reader (read-char)))))

;; @deffn {Procedure} make-space-skipper chset => proc
;; This routine will generate a reader to skip whitespace.
;; @end deffn
(define (make-space-skipper chset)
  (lambda (ch)
    (if (char-set-contains? chset ch)
	(let loop ((ch (read-char)))
	  (cond
	   ((char-set-contains? chset ch)
	    (loop (read-char)))
	   (else
	    (unread-char ch)
	    #t)))
	#f)))
	 
;; @deffn {Procedure} skip-c-space ch => #f|#t
;; If @code{ch} is space, skip all spaces, then return @code{#t}, else
;; return @code{#f}.
;; @end deffn
(define skip-c-space (make-space-skipper c:ws))


;; @deffn {Procedure} make-ident-reader cs-first cs-rest => ch -> #f|string
;; For identifiers, given the char-set for first character and the char-set
;; for following characters, return a return a reader for identifiers.
;; The reader takes a character as input and returns @code{#f} or @code{string}.
;; This will generate exception on @code{#<eof>}.
;; @end deffn
(define (make-ident-reader cs-first cs-rest)
  (lambda (ch)
    (if (char-set-contains? cs-first ch)
	(let loop ((chl (list ch)) (ch (read-char)))
	  (cond
	   ((eof-object? ch)
	    (if (null? chl) #f
		(lxlsr chl)))
	   ((char-set-contains? cs-rest ch)
	    (loop (cons ch chl) (read-char)))
	   (else (unread-char ch)
		 (lxlsr chl))))
	#f)))

;; @deffn {Procedure} make-ident-like-p ident-reader
;; Generate a predicate, from a reader, that determines if a string qualifies
;; as an identifier. 
;; @end deffn
;; Implementation may not be very efficient.
(define (make-ident-like-p reader)
  (lambda (s) (and (string? s)
		   (positive? (string-length s))
		   (eval-reader reader s)
		   #t)))

;; @deffn {Procedure} read-c-ident ch => #f|string
;; If ident pointer at following char, else (if #f) ch still last-read.
;; @end deffn
(define read-c-ident (make-ident-reader c:if c:ir))

;; @deffn {Procedure} like-c-ident? ch 
;; Determine if a string qualifies as a C identifier.
;; @end deffn
(define like-c-ident? (make-ident-like-p read-c-ident))

;; @deffn {Procedure} read-c$-ident ch => #f|string
;; Similar to @code{read-c-ident}: it allows initial @code{$}.
;; @end deffn
(define read-c$-ident
  (let ((cs (char-set-copy c:if)))
    (string->char-set! "$" cs)
    (make-ident-reader cs c:ir)))

;; @deffn {Procedure} like-c$-ident? ch 
;; Similar to @code{like-c-ident}: it allows initial @code{$}.
;; @end deffn
(define like-c$-ident? (make-ident-like-p read-c$-ident))

;; @deffn {Procedure} make-string-reader delim
;; Generate a reader that uses @code{delim} as delimiter for strings.
;; TODO: need to handle matlab-type strings.
;; TODO: need to handle multiple delim's (like python)
;; @end deffn
(define (make-string-reader delim) ;; #:xxx
  (lambda (ch)
    (if (eq? ch delim)
	(let loop ((cl '()) (ch (read-char)))
	  (cond ((eq? ch #\\)
		 (let ((c1 (read-char)))
		   (if (eq? c1 #\newline)
		       (loop cl (read-char))
		       (loop (cons* c1 cl) (read-char)))))
		((eq? ch delim) (cons '$string (lxlsr cl)))
		(else (loop (cons ch cl) (read-char)))))
	#f)))

;; @deffn {Procedure} read-oct => 123|#f
;; Read octal number, assuming @code{\0} have already been read.
;; Return integer.
;; @end deffn
(define read-oct
  (let ((cs:oct (string->char-set "01234567")))
    (lambda ()
      (let loop ((cv 0) (ch (read-char)) (n 0))
	(cond
	 ((eof-object? ch) cv)
	 ;;((> n 3) (unread-char ch) cv)
	 ((char-set-contains? cs:oct ch)
	  (loop (+ (* 8 cv) (- (char->integer ch) 48)) (read-char) (1+ n)))
	 (else (unread-char ch) cv))))))

;; @deffn {Procedure} read-hex => 123|#f
;; Read hex number.  Assumes prefix (e.g., "0x" has already been read).
;; Returns integer.
;; @end deffn
(define read-hex
  (let ((cs:dig (string->char-set "0123456789"))
	(cs:uhx (string->char-set "ABCDEF"))
	(cs:lhx (string->char-set "abcdef")))
    (lambda ()
      (let loop ((cv 0) (ch (read-char)) (n 0))
	(cond
	 ((eof-object? ch) cv)
	 ;;((> n 2) (unread-char ch) cv)
	 ((char-set-contains? cs:dig ch)
	  (loop (+ (* 16 cv) (- (char->integer ch) 48)) (read-char) (1+ n)))
	 ((char-set-contains? cs:uhx ch)
	  (loop (+ (* 16 cv) (- (char->integer ch) 55)) (read-char) (1+ n)))
	 ((char-set-contains? cs:lhx ch)
	  (loop (+ (* 16 cv) (- (char->integer ch) 87)) (read-char) (1+ n)))
	 (else (unread-char ch) cv))))))

;; @deffn {Procedure} c-escape seed
;; After @code{\\} in a C string, read the rest of the sequence and cons
;; the character, if exists, with the seed (a list).  Remember that @code{\n}
;; should, and will, just return the seed.
;; @end deffn
(define (c-escape seed)
  (let* ((ch (read-char)))
    (case ch
      ((#\newline) seed)
      ((#\\) (cons #\\ seed))
      ((#\") (cons #\" seed))
      ((#\') (cons #\' seed))
      ((#\n) (cons #\newline seed))
      ((#\r) (cons #\return seed))
      ((#\b) (cons #\bs seed))
      ((#\t) (cons #\tab seed))
      ((#\f) (cons #\page seed))
      ((#\a) (cons #\bel seed))	      ; guile 1.8 doesn't know #\alarm
      ((#\v) (cons #\vt seed))	      ; guile 1.8 doesn't know #\vtab
      ((#\0) (cons (integer->char (read-oct)) seed))
      ((#\1 #\2 #\3 #\4 #\5 #\6 #\7)
       (unread-char ch) (cons (integer->char (read-oct)) seed))
      ((#\x) (cons (integer->char (read-hex)) seed))
      (else (cons ch seed)))))

;; @deffn {Procedure} read-c-string ch => ($string . "foo")
;; Read a C-code string.  Output to code is @code{write} not @code{display}.
;; Return #f if @var{ch} is not @code{"}. @*
;; TODO: parse trigraphs
;; ??=->#, ??/->\, ??'->^, ??(->[, ??)->], ??~->|, ??<->{, ??>->}, ??-->~
;; and digraphs <:->[, :>->], <%->{ %>->} %:->#
;; @end deffn
(define (read-c-string ch)
  (if (not (eq? ch #\")) #f
      (let loop ((cl '()) (ch (read-char)))
	(cond ((eq? ch #\\) (loop (c-escape cl) (read-char)))
	      ((eq? ch #\") (cons '$string (lxlsr cl)))
	      (else (loop (cons ch cl) (read-char)))))))

;; @deffn {Procedure} make-chlit-reader
;; Generate a reader for character literals. NOT DONE.
;; For C, this reads @code{'c'} or @code{'\n'}.
;; @end deffn
(define (make-chlit-reader . rest) (error "NOT IMPLEMENTED"))

;; @deffn {Procedure} read-c-chlit ch
;; @example
;; ... 'c' ... => (read-c-chlit #\') => '($chlit . #\c)
;; @end example
;; This will return @code{$chlit}, $code{$chlit/L} for @code{wchar_t},
;; @code{$chlit/u} for @code{char16_t}, or @code{$chlit/U} for @code{char32_t}.
;; @end deffn
(define (read-c-chlit ch)
  (define (read-esc-char)
    (let ((c2 (read-char)))
      (case c2
	((#\t) "\t")		   ; horizontal tab U+0009
	((#\n) "\n")		   ; newline U+000A
	((#\v) "\v")		   ; verticle tab U+000B
	((#\f) "\f")		   ; formfeed U+000C
	((#\r) "\r")		   ; return U+000D
	((#\a) "\x07")		   ; alert U+0007
	((#\b) "\x08")		   ; backspace U+0008 not in guile 1.8
	((#\0) (string (integer->char (read-oct)))) ; octal
	((#\1 #\2 #\3 #\4 #\5 #\6 #\7)		    ; octal
	 (unread-char c2) (string (integer->char (read-oct))))
	((#\x) (string (integer->char (read-hex)))) ; hex
	((#\\ #\' #\" #\? #\|) (string c2))
	(else (error "bad escape sequence" c2)))))
  (define (wchar t)
    (case t ((#\L) '$chlit/L) ((#\u) '$chlit/u) ((#\U) '$chlit/U)))
  (cond
   ((char=? ch #\')
    (let* ((c1 (read-char))
	   (sc (if (eqv? c1 #\\) (read-esc-char) (string c1))))
      (if (not (char=? #\' (read-char))) (error "bad char lit"))
      (cons '$chlit sc)))
   ((char-set-contains? c:cx ch)
    (let ((c1 (read-char)))
      (cond
       ((char=? c1 #\') (cons (wchar ch) (cdr (read-c-chlit c1))))
       (else (unread-char c1) #f))))
   (else #f)))

(define (fix-dot l) (if (char=? #\. (car l)) (cons #\0 l) l))

;; @deffn {Procedure} make-num-reader => (proc ch) => #f|fixed|float
;; Where @emph{fixed} is @code{($fixed . "1")} and @emph{float} is
;; @code{($float . "1.0")}
;; This procedure reads C numeric literals (included fixed-point types).
;; Some literals are cleaned: @code{"0"} may be added before or after dot.
;; This allows use of @code{0b} prefix for binary literals even though that
;; is not C.
;; @end deffn
(define (make-num-reader)
  ;; This will incorrectly parse 123LUL.  Does not handle hex floats.
  ;; 0: start; 1: p-i; 2: p-f; 3: p-e-sign; 4: p-e-d; 5: packup
  ;; Now handles fixed-point (returning '$fixpt)
  (lambda (ch1)
    ;; chl: char list; ty: '$fixed or '$float; st: state; ch: next ch; ba: base
    (let loop ((chl '()) (ty #f) (ba 10) (st 0) (ch ch1))
      (case st
	((0)
	 (cond
	  ((eof-object? ch) (loop chl ty ba 5 ch))
	  ((char=? #\0 ch) (loop (cons ch chl) '$fixed 8 10 (read-char))) 
	  ((char-numeric? ch) (loop chl '$fixed ba 1 ch))
	  ((char=? #\. ch) (loop (cons ch chl) #f ba 15 (read-char))) 
	  (else #f)))
	((10) ;; allow x, b (C++14) after 0
	 (cond
	  ((eof-object? ch) (loop chl ty ba 5 ch))
	  ((char=? #\x ch) (loop (cons ch chl) ty 16 1 (read-char)))
	  ((char=? #\X ch) (loop (cons ch chl) ty 16 1 (read-char)))
	  ((char=? #\b ch) (loop (cons ch chl) ty 2 1 (read-char)))
	  (else (loop chl ty ba 1 ch))))
	((15) ;; got `.' only
	 (cond
	  ((eof-object? ch) (unread-char ch) #f)
	  ((char-numeric? ch) (loop (cons ch chl) '$float ba 2 (read-char)))
	  (else (unread-char ch) #f)))
	((1)
	 (cond
	  ((eof-object? ch) (loop chl ty ba 5 ch))
	  ((char-numeric? ch) (loop (cons ch chl) ty ba 1 (read-char)))
	  ((char=? #\. ch) (loop (cons #\. chl) '$float ba 2 (read-char)))
	  ((and (= ba 16) (char-set-contains? c:hx ch))
	   (loop (cons ch chl) ty ba 1 (read-char)))
	  ((char-set-contains? c:sx ch)
	   (loop (cons ch chl) '$fixed ba 11 (read-char)))
	  ((char-set-contains? c:nx ch)
	   (loop (cons ch chl) '$float ba 3 (read-char)))
	  ((char-set-contains? c:px ch)
	   (loop (cons ch chl) '$fixpt ba 11 (read-char)))
	  ((char-set-contains? c:if ch)
	   (sf "\nchl=~S ch=~S ty=~S ba=~S\n" chl ch ty ba)
	   (error "lex/num-reader st=1"))
	  (else (loop chl '$fixed ba 5 ch))))
	((11) ;; got lLuU suffix, look for a second
	 (cond
	  ((eof-object? ch) (cons ty (lxlsr chl)))
	  ((char-set-contains? c:sx ch)
	   (loop (cons ch chl) ty ba st (read-char)))
	  ((char-set-contains? c:px ch)
	   (loop (cons ch chl) '$fixpt ba st (read-char)))
	  (else (loop chl ty ba 5 ch))))
	#;((12) ;; got lLuU suffix, look for a third
	 (cond
	  ((eof-object? ch) (cons '$fixed (lxlsr chl)))
	  ((char-set-contains? c:sx ch)
	   (loop (cons ch chl) '$fixed ba 5 (read-char)))
	  (else (loop chl '$fixed ba 5 ch))))
	((2)
	 (cond
	  ((eof-object? ch) (loop chl ty ba 5 ch))
	  ((char-numeric? ch) (loop (cons ch chl) ty ba 2 (read-char)))
	  ((char-set-contains? c:nx ch)
	   (loop (cons ch (fix-dot chl)) ty ba 3 (read-char)))
	  ((char-set-contains? c:px ch)
	   (loop (cons ch chl) '$fixpt ba st (read-char)))
	  ((char-set-contains? c:fx ch)
	   (cons '$float (lxlsr (cons ch (fix-dot chl)))))
	  ((char-set-contains? c:if ch) (error "lex/num-reader st=2"))
	  (else (loop (fix-dot chl) ty ba 5 ch))))
	((3)
	 (cond
	  ((eof-object? ch) (loop chl ty ba 5 ch))
	  ((or (char=? #\+ ch) (char=? #\- ch))
	   (loop (cons ch chl) ty ba 4 (read-char)))
	  ((char-numeric? ch) (loop chl ty ba 4 ch))
	  (else (error "lex/num-reader st=3"))))
	((4)
	 (cond
	  ((eof-object? ch) (loop chl ty ba 5 ch))
	  ((char-numeric? ch) (loop (cons ch chl) ty ba 4 (read-char)))
	  ((char-set-contains? c:if ch) (error "lex/num-reader st=4"))
	  (else (loop chl ty ba 5 ch))))
	((5)
	 (unless (eof-object? ch) (unread-char ch))
	 (cons ty (lxlsr chl)))))))

;; @deffn {Procedure} cnumstr->scm C99-str => scm-str
;; Convert C number-string (e.g, @code{0x123LL}) to Scheme numbers-string
;; (e.g., @code{#x123}).
;; This probably belongs in @code{(nyacc lang util)}.
;; @end deffn
(define (cnumstr->scm str)
  (define (2- n) (1- (1- n))) (define (3- n) (1- (2- n)))
  (let* ((nd (string-length str)))
    (define (trim-rt st) ;; trim U, UL, ULL (and lowercase) from right
      (if (char-set-contains? c:sx (string-ref str (1- nd)))
	  (if (char-set-contains? c:sx (string-ref str (2- nd)))
	      (if (char-set-contains? c:sx (string-ref str (3- nd)))
		  (substring str st (3- nd))
		  (substring str st (2- nd)))
	      (substring str st (1- nd)))
	  (substring str st nd)))
    (if (< nd 2) str
	(if (char=? #\0 (string-ref str 0))
	    (cond
	     ((char=? #\x (string-ref str 1))
	      (string-append "#x" (trim-rt 2)))
	     ((char=? #\X (string-ref str 1))
	      (string-append "#x" (trim-rt 2)))
	     ((char=? #\b (string-ref str 1))
	      (string-append "#b" (trim-rt 2)))
	     ((char-numeric? (string-ref str 1))
	      (string-append "#o" (trim-rt 1)))
	     (else (trim-rt 0)))
	    (trim-rt 0)))))

;; @deffn {Procedure} read-c-num ch => #f|string
;; Reader for unsigned numbers as used in C (or close to it).
;; @end deffn
(define read-c-num (make-num-reader))

;;.@deffn {Procedure} si-map string-list ix => a-list
;; Convert list of strings to alist of char at ix and strings.
;; This is a helper for make-tree.
;; @end deffn
(define (si-map string-list ix)
  (let loop ((sal '()) (sl string-list))
    (cond
     ((null? sl) sal)
     ((= ix (string-length (car sl)))
      (loop (reverse (acons 'else (car sl) sal)) (cdr sl)))
     ((assq (string-ref (car sl) ix) sal) =>
      (lambda (pair)
        (set-cdr! pair (cons (car sl) (cdr pair)))
        (loop sal (cdr sl))))
     (else ;; Add (#\? . string) to alist.
      (loop (cons (cons (string-ref (car sl) ix) (list (car sl))) sal)
            (cdr sl))))))

;;.@deffn {Procedure} make-tree strtab -> tree
;; This routine takes an alist of strings and symbols and makes a tree
;; that parses one char at a time and provide @code{'else} entry for
;; signaling sequence found.  That is, if @code{("ab" . 1)} is an entry
;; then a chseq-reader (see below) would stop at @code{"ab"} and
;; return @code{1}.
;; @end deffn
(define (make-tree strtab)
  (define (si-cnvt string-list ix)
    (map (lambda (pair)
	   (if (pair? (cdr pair))
	       (cons (car pair) (si-cnvt (cdr pair) (1+ ix)))
	       (cons (car pair) (assq-ref strtab (cdr pair)))))
	 (si-map string-list ix)))
  (si-cnvt (map car strtab) 0))

;; @deffn {Procedure} make-chseq-reader strtab
;; Given alist of pairs (string, token) return a function that eats chars
;; until (token . string) is returned or @code{#f} if no match is found.
;; @end deffn
(define (make-chseq-reader strtab)
  ;; This code works on the assumption that the else-part is always last
  ;; in the list of transitions.
  (let ((tree (make-tree strtab)))
    (lambda (ch)
      (let loop ((cl (list ch)) (node tree))
	(cond
	 ((assq-ref node (car cl)) => ;; accept or shift next character
	  (lambda (n)
	    (if (eq? (caar n) 'else) ; if only else, accept, else read on
		(cons (cdar n) (lxlsr cl))
		(loop (cons (read-char) cl) n))))
	 ((assq-ref node 'else) => ; else exists, accept
	  (lambda (tok)
	    (unless (eof-object? (car cl)) (unread-char (car cl)))
	    (cons tok (lxlsr (cdr cl)))))
	 (else ;; reject
	  (let pushback ((cl cl))
	    (unless (null? (cdr cl))
	      (unless (eof-object? (car cl)) (unread-char (car cl)))
	      (pushback (cdr cl))))
	  #f))))))

;; @deffn {Procedure} make-comm-reader comm-table [#:eat-newline #t] => \
;;   ch bol -> ('$code-comm "..")|('$lone-comm "..")|#f
;; comm-table is list of cons for (start . end) comment.
;; e.g. ("--" . "\n") ("/*" . "*/")
;; test with "/* hello **/"
;; If @code{eat-newline} is specified as true then for read comments 
;; ending with a newline a newline swallowed with the comment.
;; The returned procedure has signature
;; @code{(proc ch #:optional bol #:skip-prefix #t|#f)}
;; @* Note: assumes backslash is never part of the end
;; @end deffn
(define* (make-comm-reader comm-table #:key eat-newline)

  (define (mc-read-char) ;; CHECK THIS
    (let ((ch (read-char)))
      (if (eqv? ch #\\)
	  (let ((ch (read-char)))
	    (if (eqv? ch #\newline)
		(read-char)
		(begin (unread-char ch) #\\)))
	  ch)))

  ;; Skip whitespace upto columm @var{col}, and fill in partial tab, if needed.
  ;; @example
  ;; (skip-ws-to-col 4 "    def" '(#\newline) => (#\d #\newline)
  ;; (skip-ws-to-col 6 "\tdef" '(#\newline)) => (#\d #\space #\space #\newline)
  ;; @end example
  (define (skip-to-col col il)
    (let loop ((il il) (cc 0) (ch (read-char)))
      ;;(simple-format #t " skip-to-col loop il=~S cc=~S ch=~S\n" il cc ch)
      (cond
       ((= cc col) (cons ch il))
       ((> cc col) (loop (cons ch il) (1- cc) #\space)) ; tab over-run
       ((char=? ch #\space) (loop il (1+ cc) (read-char)))
       ((char=? ch #\tab) (loop il (* 8 (quotient (+ cc 9) 8)) (read-char)))
       (else (cons ch il)))))
	 
  (let ((tree (make-tree comm-table)))
    (lambda* (ch #:optional bol #:key skip-prefix)
      (letrec
	  ((scol (1- (port-column (current-input-port)))) ;; 1- since ch read
	   (tval (if bol '$lone-comm '$code-comm))
	   (match-beg ;; match start of comment, return end-string
	    (lambda (cl node)
	      (cond
	       ((assq-ref node (car cl)) => ;; shift next character
		(lambda (n) (match-beg (cons (mc-read-char) cl) n)))
	       ((assq-ref node 'else) =>
		(lambda (res) (unread-char (car cl)) res)) ; yuck?
	       (else
		(let pushback ((cl cl))
		  (unless (null? (cdr cl))
		    (unread-char (car cl))
		    (pushback (cdr cl))))
		#f))))
	   (find-end ;; find end of comment, return comment
	    ;; cl: comm char list (cleared from ps);
	    ;; sl: shift list (matched from ps); il: input list;
	    ;; ps: pattern string (e.g., "*/") ; px: pattern index;
	    (lambda (cl sl il ps px)
	      (cond
	       ((eq? px (string-length ps)) ; can Guile optimize this?
		(if (and (not eat-newline) (eq? #\newline (car sl)))
		    (unread-char #\newline))
		(if (and (pair? cl) (eqv? (car cl) #\cr))
		    (cons tval (lxlsr (cdr cl))) ; remove trailing \r 
		    (cons tval (lxlsr cl))))
	       ((null? il) (find-end cl sl (cons (mc-read-char) il) ps px))
	       ((eof-object? (car il))
		(if (char=? (string-ref ps px) #\newline) (cons tval (lxlsr cl))
		    (throw 'nyacc-error "open comment")))
	       ((eqv? (car il) (string-ref ps px))
		(find-end cl (cons (car il) sl) (cdr il) ps (1+ px)))
	       ((and (char=? (car il) #\newline) skip-prefix)
		;; assumes newline can only be end of ps
		;;(simple-format #t "cl=~S sl=~S il=~S\n" cl sl il)
		(find-end (cons #\newline (append sl cl)) '()
			  (skip-to-col scol (cdr il)) ps 0))
	       (else
		(let ((il1 (append-reverse sl il)))
		  (find-end (cons (car il1) cl) '() (cdr il1) ps 0)))))))
	(let ((ep (match-beg (list ch) tree))) ;; ep=end pattern (e.g., "*/")
	  (if ep (find-end '() '() (list (mc-read-char)) ep 0) #f))))))

(define read-c-comm (make-comm-reader '(("/*" . "*/") ("//" . "\n"))))

;; @deffn {Procedure} filter-mt p? al => al
;; Filter match-table based on cars of al.
;; @end deffn
(define (filter-mt p? al) (filter (lambda (x) (p? (car x))) al))

;; @deffn {Procedure} remove-mt p? al => al
;; Remove match-table based on cars of al.
;; @end deffn
(define (remove-mt p? al) (remove (lambda (x) (p? (car x))) al))

;; @deffn {Procedure} map-mt f al => al
;; Map cars of al.
;; @end deffn
(define (map-mt f al) (map (lambda (x) (cons (f (car x)) (cdr x))) al))

;; @deffn {Procedure} make-lexer-generator match-table => lexer-generator
;; @example
;; (define gen-lexer (make-lexer-generator #:ident-reader my-id-rdr))
;; (with-input-from-file "foo" (parse (gen-lexer)))
;; @end example
;;
;; Return a thunk that returns tokens.
;; Change this to have user pass the following routines (optionally?)
;; read-num, read-ident, read-comm
;; reztab = reserved ($ident, $fixed, $float ...
;; chrtab = characters
;; comm-reader : if parser does not deal with comments must return #f
;;               but problem with character ..
;; extra-reader: insert an user-defined reader
;; match-table:
;; @enumerate
;; symbol -> (string . symbol)
;; reserved -> (symbol . symbol)
;; char -> (char . char)
;; @end enumerate
;; todo: add bol status
;; todo: maybe separate reading of keywords from identifiers: (keywd ch) =>
;; @end deffn
(define* (make-lexer-generator match-table
			       #:key
			       ident-reader num-reader
			       string-reader chlit-reader
			       comm-reader comm-skipper
			       space-chars extra-reader)
  (let* ((read-ident (or ident-reader (make-ident-reader c:if c:ir)))
	 (read-num (or num-reader (make-num-reader)))
	 (read-string (or string-reader (make-string-reader #\")))
	 (read-chlit (or chlit-reader (lambda (ch) #f)))
	 (read-comm (or comm-reader (lambda (ch bol) #f)))
	 (skip-comm (or comm-skipper (lambda (ch) #f)))
	 (spaces (or space-chars " \t\r\n"))
	 (space-cs (cond ((string? spaces) (string->char-set spaces))
			 ((list? spaces) (list->char-set spaces))
			 ((char-set? spaces) spaces)
			 (else (error "expecting string list or char-set"))))
	 (read-extra (or extra-reader (lambda (ch) #f)))
	 ;;
	 (ident-like? (make-ident-like-p read-ident))
	 ;;
	 (strtab (filter-mt string? match-table)) ; strings in grammar
	 (kwstab (filter-mt ident-like? strtab))  ; keyword strings =>
	 (keytab (map-mt string->symbol kwstab))  ; keywords in grammar
	 (chrseq (remove-mt ident-like? strtab))  ; character sequences
	 (symtab (filter-mt symbol? match-table)) ; symbols in grammar
	 (chrtab (filter-mt char? match-table))	  ; characters in grammar
	 ;;
	 (read-chseq (make-chseq-reader chrseq))
	 (assc-$ (lambda (pair) (cons (assq-ref symtab (car pair)) (cdr pair)))))
    (lambda ()
      (let ((bol #f))
	(lambda ()
	  (let loop ((ch (read-char)))
	    (cond
	     ((eof-object? ch) (assc-$ (cons '$end ch)))
	     ;;((eq? ch #\newline) (set! bol #t) (loop (read-char)))
	     ((read-extra ch))
	     ((char-set-contains? space-cs ch) (loop (read-char)))
	     ((and (eqv? ch #\newline) (set! bol #t) #f))
	     ((read-comm ch bol) =>
	      (lambda (p) (set! bol #f) (assc-$ p)))
	     ((skip-comm ch) (loop (read-char)))
	     ((read-num ch) => assc-$)	  ; => $fixed or $float
	     ((read-string ch) => assc-$) ; => $string
	     ((read-chlit ch) => assc-$)  ; => $chlit
	     ((read-ident ch) =>
	      (lambda (s) (or (and=> (assq-ref keytab (string->symbol s))
				     (lambda (tval) (cons tval s)))
			      (assc-$ (cons '$ident s)))))
	     ((read-chseq ch) => identity)
	     ((assq-ref chrtab ch) => (lambda (t) (cons t (string ch))))
	     (else (cons ch ch))))))))) ; should be error

;; @end table

;; --- last line ---

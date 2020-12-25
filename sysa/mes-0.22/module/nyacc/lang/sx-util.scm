;;; module/nyacc/sx-util.scm - runtime utilities for the parsers

;; Copyright (C) 2015-2018 Matthew R. Wette
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this library; if not, see <http://www.gnu.org/licenses/>.

;;; Notes:

;; The syntax of SXML trees is simple:
;; @example
;; expr => (tag item @dots{}) | (tag (@@ attr @dots{}) item @dots{})
;; item => string | expr
;; attr => (tag . string)
;; @end example


;;; Code:

(define-module (nyacc lang sx-util)
  #:export (make-sx
	    sx-tag sx-attr sx-tail sx-length sx-ref sx-ref*
	    sx-has-attr? sx-attr-ref sx-attr-add sx-attr-add* sx-attr-set!
	    sx-find
	    sx-split sx-split* sx-join sx-join* sx-cons* sx-list
	    sx-unitize
	    sx-match)
  #:use-module ((srfi srfi-1) #:select (find fold fold-right append-reverse)))
(cond-expand
  (mes)
  (guile-2)
  (guile (use-modules (srfi srfi-16)))
  (else))

;; === sx ==============================
;; @section SXML Utility Procedures
;; Some lot of these look like existing Guile list procedures (e.g.,
;; @code{sx-tail} versus @code{list-tail} but in sx lists the optional
;; attributea are `invisible'. For example, @code{'(elt (@abc) "d")}
;; is an sx of length two: the tag @code{elt} and the payload @code{"d"}.

;; @deffn {Procedure} sx-expr? expr
;; This predicate checks if @var{expr} looks like a valid SXML form.
;; It is not exhaustive: @var{expr} is checked to be a list with first
;; element a symbol.
;; @end deffn
(define (sxml-expr? sx)
  (and (pair? sx) (symbol? (car sx)) (list? sx)))

;; @deffn {Procedure} make-sx tag attr . elts
;; This will build an SXML expression from the symbolic tag, optional
;; attributes and elements.  The attributes @var{attr} can be of the from
;; @code{(@ (key "val") ...)} or @code{((key "val") ...)}.  If elements
;; in @var{elts} are not pairs or strings they are ignored, so elmeents of
;; @var{elts} of the form @code{#f} and @code{'()} will not end up in the
;; returned SXML form.
;; @end deffn
(define (make-sx tag attr . elts)
  (let ((tail (fold-right
	       (lambda (elt sx)
		 (cond
		  ((pair? elt) (cons elt sx))
		  ((string? elt) (cons elt sx))
		  (else sx)))
	       '() elts)))
    (if (pair? attr)
	(if (eq? '@ (car attr))
	    (cons* tag attr tail)
	    (cons* tag `(@ . ,attr) tail))
	(cons tag tail))))

;; @deffn {Procedure} sx-length sx => <int>
;; Return the length, don't include attributes, but do include tag
;; @end deffn
(define (sx-length sx)
  (let ((ln (length sx)))
    (cond
      ((zero? ln) 0)
      ((= 1 ln) 1)
      ((not (pair? (cadr sx))) ln)
      ((eq? '@ (caadr sx)) (1- ln))
      (else ln))))

;; @deffn {Procedure} sx-tag sx => tag
;; Return the tag for a tree
;; @end deffn
(define (sx-tag sx)
  (if (pair? sx) (car sx) #f))

;; @deffn {Procedure} sx-ref sx ix => item
;; Reference the @code{ix}-th element of the list, not counting the optional
;; attributes item.  If the list is shorter than the index, return @code{#f}.
;; [note to author: The behavior to return @code{#f} if no elements is not
;; consistent with @code{list-ref}.  Consider changing it.  Note also there
;; is never a danger of an element being @code{#f}.]
;; @example
;; (sx-ref '(abc 1) => #f
;; (sx-ref '(abc "def") 1) => "def"
;; (sx-ref '(abc (@ (foo "1")) "def") 1) => "def"
;; @end example
;; @end deffn
(define (sx-ref sx ix)
  (define (list-xref l x) (if (> (length l) x) (list-ref l x) #f))
  (cond
   ((zero? ix) (car sx))
   ((null? (cdr sx)) #f)
   ((and (pair? (cadr sx)) (eqv? '@ (caadr sx)))
    (list-xref sx (1+ ix)))
   (else
    (list-xref sx ix))))

;; @deffn {Procedure} sx-ref* sx ix1 ix2 ... => item
;; Equivalent to
;; @example
;; (((sx-ref (sx-ref sx ix1) ix2) ...) ...)
;; @end example
;; @end deffn
(define (sx-ref* sx . args)
  (fold (lambda (ix sx) (and (pair? sx) (sx-ref sx ix))) sx args))

;; @deffn {Procedure} sx-tail sx [ix] => (list)
;; Return the ix-th tail starting after the tag and attribut list, where
;; @var{ix} must be positive.  For example,
;; @example
;; (sx-tail '(tag (@ (abc . "123")) (foo) (bar)) 1) => ((foo) (bar))
;; @end example
;; Without second argument @var{ix} is 1.
;; @end deffn
(define sx-tail
  (case-lambda
   ((sx ix)
    (cond
     ((zero? ix) (error "sx-tail: expecting index greater than 0"))
     ((null? (cdr sx)) (list-tail sx ix))
     ((and (pair? (cadr sx)) (eqv? '@ (caadr sx))) (list-tail sx (1+ ix)))
     (else (list-tail sx ix))))
   ((sx)
    (sx-tail sx 1))))

;; @deffn {Procedure} sx-find tag sx => (tag ...)
;; @deffnx {Procedure} sx-find path sx => (tag ...)
;; In the first form @var{tag} is a symbolic tag in the first level.
;; Find the first matching element (in the first level).
;; In the second form, the argument @var{path} is a pair.  Apply sxpath
;; and take it's car,
;; if found, or return @code{#f}, like lxml's @code{tree.find()} method.
;; @* NOTE: the path version is currently disabled, to remove dependence
;; on the module @code{(sxml xpath)}.
;; @end deffn
(define (sx-find tag-or-path sx)
  (cond
   ((symbol? tag-or-path)
    (find (lambda (node)
	    (and (pair? node) (eqv? tag-or-path (car node))))
	  sx))
   (else
    (error "sx-find: expecting first arg to be tag or sxpath"))))

;; @deffn {Procedure} sx-has-attr? sx
;; p to determine if @arg{sx} has attributes.
;; @end deffn
(define (sx-has-attr? sx)
  (and (pair? (cdr sx)) (pair? (cadr sx)) (eqv? '@ (caadr sx)) #t))

;; @deffn {Procedure} sx-attr sx => ((k v) ...)
;; @example
;; (sx-attr '(abc (@ (foo "1")) def) 1) => ((foo "1"))
;; @end example
;; @end deffn
(define (sx-attr sx)
  (if (and (pair? (cdr sx))
	   (pair? (cadr sx))
	   (eqv? '@ (caadr sx)))
      (cdadr sx)
      '()))

;; @deffn {Procedure} sx-attr-ref sx|node|tail key => val
;; Return an attribute value given the key, or @code{#f}.
;; Also works if passed the attribute node @code{(@ ...)} or its tail.
;; @end deffn
(define (sx-attr-ref sx key)
  (let ((attr-tail (cond ((null? sx) sx)
			 ((pair? (car sx)) sx)
			 ((eqv? '@ (car sx)) (car sx))
			 ((sx-attr sx))
			 (else '()))))
    (and=> (assq-ref attr-tail key) car)))

;; @deffn {Procedure} sx-attr-add sx key-or-pair [val]
;; Add attribute to sx, passing either a key-val pair or key and val.
;; @end deffn
(define* (sx-attr-add sx key-or-pair #:optional val)
  (let* ((pair (if val (list key-or-pair val) key-or-pair))
	 (key (car pair)) (val (cadr pair)))
    (cons
     (sx-tag sx)
     (if (sx-has-attr? sx)
	 (cons `(@ pair
		   ,@(let loop ((atl (sx-attr sx)))
		       (cond ((null? atl) '())
			     ((eq? key (caar atl)) (loop (cdr atl)))
			     (else (cons (car atl) (loop (cdr atl)))))))
	       (cddr sx))
	 (cons `(@ ,pair) (cdr sx))))))

;; @deffn {Procedure} sx-attr-add* sx key val [key val [@dots{} ]] => sx
;; Add key-val pairs. @var{key} must be a symbol and @var{val} must be
;; a string.  Return a new @emph{sx}.
;; @end deffn
(define (sx-attr-add* sx . rest)
  (let* ((attrs (sx-attr sx))
	 (attrs (let loop ((kvl rest))
		  (if (null? kvl) attrs
		      (cons (list (car kvl) (cadr kvl)) (loop (cddr kvl)))))))
    (cons* (sx-tag sx) (cons '@ attrs)
	   (if (sx-has-attr? sx) (cddr sx) (cdr sx)))))

;; @deffn {Procedure} sx-attr-set! sx key val
;; Set attribute for sx.  If no attributes exist, if key does not exist,
;; add it, if it does exist, replace it.
;; @end deffn
(define (sx-attr-set! sx key val)
  (if (sx-has-attr? sx)
      (let ((attr (cadr sx)))
	(set-cdr! attr (assoc-set! (cdr attr) key (list val))))
      (set-cdr! sx (cons `(@ (,key ,val)) (cdr sx))))
  sx)

;; @deffn {Procedure} sx-cons* tag attr exp ... tail => sx
;; @deffnx {Procedure} sx-list tag attr exp ... => sx
;; Generate the tag and the attr list if it exists.  Note that
;; The following are equivalent:
;; @example
;; (sx-cons* tag attr elt1 elt2 '())
;; (sx-list tag attr elt1 elt2)
;; @end example
;; @noindent
;; Expressions that are @code{#f} or @code{'()} will be skipped;
;; they should be strings or pairs.
;; @end deffn
(define (sx-cons* tag attr . rest)
  (if (null? rest) (error "sx-cons: expecing tail"))
  (let ((attr (cond
	       ((not attr) #f)
	       ((null? attr) #f)
	       ((pair? (car attr)) `(@ . ,attr))
	       (else attr)))
	(tail (let loop ((items rest))
		(cond
		 ((null? (cdr items)) (car items))
		 ((not (car items)) (loop (cdr items)))
		 ((null? (car items)) (loop (cdr items)))
		 (else (cons (car items) (loop (cdr items))))))))
    (if attr (cons* tag attr tail) (cons tag tail))))
(define (sx-list tag attr . rest)
  (let ((attr (cond
	       ((not attr) #f)
	       ((null? attr) #f)
	       ((pair? (car attr)) `(@ . ,attr))
	       (else attr)))
	(tail (let loop ((items rest))
		(cond
		 ((null? items) '())
		 ((not (car items)) (loop (cdr items)))
		 ((null? (car items)) (loop (cdr items)))
		 (else (cons (car items) (loop (cdr items))))))))
    (if attr (cons* tag attr tail) (cons tag tail))))

;; @deffn {Procedure} sx-split sexp => tag attr tail
;; @deffnx {Procedure} sx-split* sexp => tag attr exp ...
;; Split an SXML element into its constituent parts, as a @code{values},
;; where @var{attr} is list of pairs.  If no attributes exist, @var{attr}
;; is @code{'()}.
;; @end deffn
(define (sx-split sexp)
  (let ((tag (sx-tag sexp))
	(attr (sx-attr sexp))
	(tail (sx-tail sexp)))
    (values tag attr tail)))
(define (sx-split* sexp)
  (let ((tag (sx-tag sexp))
	(attr (sx-attr sexp))
	(tail (sx-tail sexp)))
    (apply values tag attr tail)))

;; @deffn {Procedure} sx-join tag attr tail => sexp
;; @deffnx {Procedure} sx-join* tag attr exp ... => sexp
;; Build an SXML element by its parts.  If @var{ATTR} is @code{'()} skip;
;; @code{sx-join*} will remove any exp that are @code{#f} or @code{'()}.
;; @end deffn
(define (sx-join tag attr tail)
  (if (and attr (pair? attr))
      (if (pair? (car attr))
	  (cons* tag `(@ . ,attr) tail)
	  (cons* tag `(@ ,attr) tail))
      (cons tag tail)))
(define (sx-join* tag attr . tail)
  (let ((tail (let loop ((tail tail))
		(cond
		 ((null? tail) '())
		 ((not tail) (loop (cdr tail)))
		 ((null? (car tail)) (loop (cdr tail)))
		 (else (cons (car tail) (loop (cdr tail))))))))
    (if (and attr (pair? attr))
	(if (pair? (car attr))
	    (cons* tag `(@ . ,attr) tail)
	    (cons* tag `(@ ,attr) tail))
	(cons tag tail))))

;; @deffn {Procedure} sx-unitize list-tag form seed
;; Given a declaration of form @code{(tag ... (elt-list ...) ...)}
;; fold into the seed broken list of
;; @code{(tag ... elt1 ...) (tag ... elt2 ...) ... seed}.
;; Any attributes for the list form are lost.
;; @end deffn
(define (sx-unitize list-tag form seed)
  (let loop ((head '()) (elts '()) (tail '()) (form form))
    (if (null? elts)
	(if (and (pair? (car form)) (eq? list-tag (sx-tag (car form))))
	    (loop head (cdar form) (cdr form) '())
	    (loop (cons (car form) head) elts tail (cdr form)))
	(let loop2 ((elts elts))
	  (if (null? elts) seed
	      (cons (append-reverse (cons (car elts) head) tail)
		    (loop2 (cdr elts))))))))

;; ============================================================================

;; sx-match: somewhat like sxml-match but hoping to be more usable and more
;; efficient for nyacc.  Note that sxml-match used in c99/pprint has to be
;; broken up in order to not overflow the stack during compilation.
;; This uses only syntax-rules; sxml uses syntax-case.


;; sx-haz-attr? val
;;(define (sx-haz-attr? sx)
;;  (and (pair? (cdr sx)) (pair? (cadr sx)) (eqv? '@ (caadr sx)) #t))
	
;; Given that a tag must be ... we define the syntax of SXML as follows:
;; SXML is a text format for XML using S-expressions, sexp's whose first
;; element is a symbol for a legal XML tag.  The syntax is:
;;   sexp: (tag node ...) | (tag (@ sexp ...) node ...)
;;   node: sexp | *text*
;; OR
;;   sexp: (tag attl tail) | (tag tail)
;;   attl: (@ (k "v") ...)
;;   tail: (node ...)
;;   node: sexp | *text*
;; where
;;   tag is a Scheme symbol for a legal XML tag name
;;   attl is an attribute list, a list whose first element is '@
;;   tail is a list of node
;;   node is sexp or a text string.

;; patterns:
;; attribute list only specified by (@ . ,<name>) where <name> is a var name
;; Specify attribute list if you want to capture the list of attributes with
;; a binding.  Otherwise leave it out.  The only way to test for no attributes
;; is to capture the a-list and test with @code{pair?}
;; FIX THIS:
;;   (foo (@ . ,<name>) (bar ,abc) ...)
;;   (foo (bar ,abc) ...)
;;   (foo ... , *)
;;   (* ...)			any sexp
;;   *				any node (i.e., sexp or text)
;; or use `*any*'
;; need don't care: (foo a b c . ?) (foo a b ? c)
;; if expect text, must use , or ?
;; ideas:
;;   instead of (* ...) use (*any* ...)
;;   use (*text* "text") to match text or (*text* ,text)
;; kt kf are continuation syntax expresions

;; @deffn {Syntax} sx-match exp (pat body ...) ...
;; This syntax will attempt to match @var{expr} against the patterns.
;; At runtime, when @var{pat} is matched against @var{exp}, then @var{body ...}
;; will be evaluated.
;; @end deffn
(define-syntax sx-match
  (syntax-rules ()
    ((_ e c ...)
     (let ((v e)) (sx-match-1 v c ...)))))

(define-syntax sx-match-1
  (syntax-rules ()
    ((_ v (pat exp ...) c1 ...)
     (let ((kf (lambda () (sx-match-1 v c1 ...))))
       (sxm-sexp v pat (begin exp ...) (kf))))
    ((_ v) (error "sx-match: nothing matches"))))

;; sxm-sexp val pat kt kf
;; match sexp
(define-syntax sxm-sexp
  (syntax-rules (@ unquote else)
    ;; accept anything
    ((_ v (unquote w) kt kf) (let ((w v)) kt))
    ;; capture attributes
    ((_ v (tag (@ . (unquote al)) . nl) kt kf)
     (sxm-tag (car v) tag
	      (if (sx-has-attr? v)
		  (let ((al (cdadr v))) (sxm-tail (cddr v) nl kt kf))
		  (let ((al '())) (sxm-tail (cdr v) nl kt kf)))
	      kf))
    ;; ignore attributes; (cadr v) may be an attr node. If so, ignore it.
    ((_ v (tag . nl) kt kf)
     (sxm-tag (car v) tag
	      (if (sx-has-attr? v)
		  (sxm-tail (cddr v) nl kt kf)
		  (sxm-tail (cdr v) nl kt kf))
	      kf))
    ;; deprecate `else' syntax?
    ((_ v else kt kf) kt)))
 
;; sxm-tag val pat kt kf
;; match tag: foo|#(foo bar baz)|,any
(define-syntax sxm-tag
  (syntax-rules (unquote)
    ((_ tv (unquote t0) kt kf)
     (let ((t0 tv)) kt))
    ((_ tv (t0 t1 ...) kt kf)
     (if (memq tv '(t0 t1 ...)) kt kf))
    ((_ tv #(t0 t1 ...) kt kf)
     (if (memq tv '(t0 t1 ...)) kt kf))
    ((_ tv t0 kt kf)
     (if (eqv? tv 't0) kt kf))))

(define (rule-error s) (error "sx-match: rule element is not SXML" s))
(define (expr-error v) (error "sx-match: expr element is not SXML" v))

;; sxm-tail val pat kt kf
;; match tail of sexp = list of nodes
(define-syntax sxm-tail
  (syntax-rules (unquote)
    ((_ v () kt kf) (if (null? v) kt kf))
    ((_ v (unquote w) kt kf) (let ((w v)) kt))
    ((_ v (hp . tp) kt kf)
     (if (pair? v)
	 (let ((hv (car v)) (tv (cdr v)))
	   (sxm-node hv hp (sxm-tail tv tp kt kf) kf))
	 kf))
    ((_ v p kt kf) kf)))

;; [ht][vp] = [head,tail][value,pattern]
;; Can this be set up to match a string constant?
(define-syntax sxm-node
  (syntax-rules (unquote)
    ((_ v () kt kf) (if (null? v) kt kf))
    ((_ v (unquote w) kt kf) (let ((w v)) kt))
    ((_ v (hp . tp) kt kf) (if (pair? v) (sxm-sexp v (hp . tp) kt kf) kf))
    ((_ v s kt kf)
     (begin
       (unless (string? s) (rule-error s))
       (unless (string? v) (expr-error v))
       (if (string=? s v) kt kf)))
    ;;((_ v s kt kf) (if (string=? s v) kt kf))
    ;;^-- If not pair or unquote then must be string, right?
   ))

;; --- last line ---

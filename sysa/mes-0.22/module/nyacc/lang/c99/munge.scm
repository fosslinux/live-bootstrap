;;; nyacc/lang/c99/munge.scm - util's for processing output of the C99 parser

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

;; Todo:
;; 1) mdecl == munged (unwrapped) declaration
;; 2) I want a way to keep named enums in expand-typerefs.
;;    Currently, they are expanded to int.
;; 3) Usual sequence is: expand-typerefs, stripdown-udecl, udecl->mdecl.
;; 4) Unitize-decl is shallow.  It does not dive into structs and unitize.
;; 5) In expand-typerefs if need to expand `foo_t *x' then change to
;;    a) if struct use `struct foo *x;'
;;    b) if fixed/float use `int *x;' etc
;;    c> if function use `void *x;'
;; 6) Check use of comments as attributes.

;;; Code:

(define-module (nyacc lang c99 munge)
  #:export (c99-trans-unit->udict
	    c99-trans-unit->udict/deep
	    udict-ref udict-struct-ref udict-union-ref udict-enum-ref

	    ;; generating def's dict
	    c99-trans-unit->ddict udict-enums->ddict

	    ;; munging
	    expand-typerefs
	    stripdown-udecl
	    udecl-rem-type-qual specl-rem-type-qual
	    udecl->mdecl udecl->mdecl/comm mdecl->udecl

	    unwrap-decl
	    canize-enum-def-list
	    fixed-width-int-names

	    typedef-decl?
	    
	    unitize-decl unitize-comp-decl unitize-param-decl
	    declr-ident declr-id decl-id
	    iter-declrs
	    split-decl split-udecl

	    clean-field-list clean-fields
	    inc-keeper?

	    ;; deprecated
	    udecl->mspec udecl->mspec/comm mspec->udecl
	    tree->udict tree->udict/deep
	    declr->ident
	    match-decl match-comp-decl match-param-decl
	    expand-decl-typerefs
	    fix-fields
	    ;; debugging
	    stripdown-1
	    tdef-splice-specl
	    tdef-splice-declr)
  #:use-module ((nyacc lang c99 cpp) #:select (eval-cpp-expr))
  #:use-module (nyacc lang c99 cxeval) ;; eval-c99-cx
  #:use-module (nyacc lang c99 pprint)
  #:use-module (nyacc lang c99 util)
  #:use-module (nyacc lang util)
  #:use-module (nyacc lang sx-util)
  #:use-module ((sxml fold) #:select (foldts foldts*))
  #:use-module (sxml match)
  #:use-module (srfi srfi-11)		; let-values
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-1)
  #:use-module (system base pmatch)
  ;; debugging:
  #:use-module (system vm trace)
  #:use-module (ice-9 pretty-print))
;; undocumented Guile builtins: or-map

(define (sferr fmt . args) (apply simple-format (current-error-port) fmt args))
(define (pperr exp)
  (pretty-print exp (current-error-port) #:per-line-prefix "  "))
(define OA object-address)

(define tmap-fmt
  '(("char" "%hhd")
    ("unsigned char" "%hhu")
    ("short int" "%hd")
    ("unsigned short int" "%hu")
    ("int" "%d")
    ("unsigned int" "%u")
    ("long int" "%ld")
    ("unsigned long int" "%lu")
    ("long long int" "%lld")
    ("unsigned long long int" "%llu")))

;; @deffn {Procedure} attr-append . attr-lists
;; This is hack for now: it is @code{append}. It needs to be set up to combine
;; @code{attributes} and @code{comment}.
;; @end deffn
;; TODO FIXME
(define attr-append append)

;; @deffn {Procedure} pointer-declr? declr
;; This predictate indicates if @var{declr} is a pointer.
;; Does not handle @code{(*ftn)()}.
;; The argument can also be @code{init-declr-list} or @code{comp-declr-list}
;; in which case all elements need to be pointers.
;; @end deffn
(define (pointer-declr? declr)
  ;;(sferr "pointer-declr? ~S\n" declr)
  (and
   declr
   (sx-match declr
     ((init-declr ,declr) (pointer-declr? declr))
     ((comp-declr ,declr) (pointer-declr? declr))
     ((param-declr ,declr) (pointer-declr? declr))
     ;;
     ((ptr-declr ,pointer ,dir-declr) #t)
     ((array-of . ,rest) #t)
     ((ftn-declr . ,rest) #t)
     ((abs-declr (pointer . ,r1) . ,r2) #t)
     ;;
     ((init-declr-list . ,declrs)
      (fold (lambda (dcl seed) (and (pointer-declr? dcl) seed)) #t declrs))
     ((comp-declr-list . ,declrs)
      (fold (lambda (dcl seed) (and (pointer-declr? dcl) seed)) #t declrs))
     ;;
     (else #f))))

;; @deffn {Procedure} pointer-pass-declr? declr => #t|#f
;; This predicate determines if the declarator is implemented as a pointer.
;; That is, it is an explicit pointer, an array (ERROR), or a function.
;; @end deffn
(define (pointer-pass-declr? declr)
  (and
   declr
   (sx-match declr
     ((init-declr ,declr) (pointer-declr? declr))
     ((comp-declr ,declr) (pointer-declr? declr))
     ((param-declr ,declr) (pointer-declr? declr))
     ;;
     ((ptr-declr ,pointer ,dir-declr) #t)
     ((array-of . ,rest) #t)
     ((ftn-declr . ,rest) #t)
     ((abs-declr (pointer . ,r1) . ,r2) #t)
     ;;
     ((init-declr-list . ,declrs)
      (fold (lambda (dcl seed) (and (pointer-declr? dcl) seed)) #t declrs))
     ((comp-declr-list . ,declrs)
      (fold (lambda (dcl seed) (and (pointer-declr? dcl) seed)) #t declrs))
     ;;
     (else #f))))

;; Use the term @dfn{udecl}, or unit-declaration, for a declaration which has
;; only one decl-item.  That is where,
;; @example
;; @end example
;; (decl (decl-spec-list ...) (init-declr-list (init-declr ...) ...))
;; @noindent
;; has been replaced by
;; (decl (decl-spec-list ...) (init-declr ...))
;; ...
;; @example
;; @end example

;; mdecl is
;; ("foo" (pointer-to) (array-of 3) (fixed-type "unsigned int"))
;; which can be converted to
;; ("(*foo) (array-of 3) (fixed-type "unsigned int"))
;; which can be converted to
;; (("((*foo)[0])" (fixed-type "unsigned int"))
;;  ("((*foo)[1])" (fixed-type "unsigned int"))
;;  ("((*foo)[2])" (fixed-type "unsigned int"))

;; may need to replace (typename "int32_t") with (fixed-type "int32_t")

;; @deffn {Procedure} inc-keeper? tree inc-filter => #f| tree
;; This is a helper.  @var{inc-filter} is @code{#t}, @code{#f} or a
;; precicate procedure, which given the name of the file, determines if
;; it should be processed.
;; @end deffn
(define (inc-keeper? tree filter)
  (if (and (eqv? (sx-tag tree) 'cpp-stmt)
	   (eqv? (sx-tag (sx-ref tree 1)) 'include)
	   (pair? (sx-ref* tree 1 2))
	   (if (procedure? filter)
	       (let ((incl (sx-ref* tree 1 1))
		     (path (sx-attr-ref (sx-ref tree 1) 'path)))
		 (filter incl path))
	       filter))
      (sx-ref* tree 1 2)
      #f))

;; @deffn {Procedure} c99-trans-unit->udict tree [seed] [#:inc-filter f]
;; @deffnx {Procedure} c99-trans-unit->udict/deep tree [seed]
;; Convert a C parse tree into a assoc-list of global names and definitions.
;; This will unwrap @code{init-declr-list} into list of decls w/
;; @code{init-declr}.
;; The declarations come reversed from order in file!
;; @example
;; BUG: need to add struct and union defn's: struct foo { int x; };
;; how to deal with this
;; lookup '(struct . "foo"), "struct foo", ???
;; wanted "struct" -> dict but that is not great
;; solution: unitize-decl => '(struct . "foo") then filter to generate
;; ("struct" ("foo" . decl) ("bar" . decl) ...)
;; ("union" ("bar" . decl) ("bar" . decl) ...)
;; ("enum" ("" . decl) ("foo" . decl) ("bar" . decl) ...)
;; @end example
;; So globals could be in udict, udefs or anon-enum.
;; @example
;; What about anonymous enums?  And enums in general?
;; Anonmous enum should be expaneded into 
;; @end example
;; @noindent
;; Notes:
;; @itemize
;; @item
;; If @var{tree} is not a pair then @var{seed} -- or @code{'()} -- is returned.
;; The inc-filter @var{f} is either @code{#t}, @code{#f} or predicate procedure
;; of one argument, the include path, to indicate whether it should be included
;; in the dictionary.
;; @item
;; If this routine is called multiple times on the same tree the u-decl's will
;; not be @code{eq?} since the top-level lists are generated on the fly.
;; (See @code{unitize-decl}.)
;; @end itemize
;; @end deffn
(define* (c99-trans-unit->udict tree #:optional (seed '()) #:key inc-filter)
  (if (pair? tree)
      (fold-right
       (lambda (tree seed)
	 (cond
	  ((eqv? (sx-tag tree) 'decl)
	   (unitize-decl tree seed))
	  ((inc-keeper? tree inc-filter) =>
	   (lambda (inc-tree)
	     (c99-trans-unit->udict inc-tree seed #:inc-filter inc-filter)))
	  (else seed)))
       seed
       (cdr tree))
      seed))
(define (c99-trans-unit->udict/deep tree)
  (c99-trans-unit->udict tree #:inc-filter #t))

;; @deffn {Procedure} iter-declrs tag specl declrs seed
;; @deffnx {Procedure} iter-declrs-right tag specl declrs tail seed
;; This is a support procedure for the munge routines.  If no decl'rs
;; then @var{declrs} should be #f.
;; Since @code{trans-unit->udict} is fold-right, this must be fold-right.
;; @end deffn
(define (iter-declrs tag attr specl declrs seed)
  (if (pair? declrs)
      (fold-right
       (lambda (declr seed)
	 (acons (declr-id declr) (sx-list tag attr specl declr) seed))
       seed declrs)
      seed))

;; @deffn {Procedure} split-decl decl => values
;; This routine splits a declaration (or comp-decl or param-decl) into
;; its constituent parts.  Attributes are currently not passed.
;; Get @code{(values tag spec-l declrs)}.  If the declrator is
;; already unitized you get that, else the list (w/o tag) of declarators.
;; @example
;;   (split-decl
;;      '(decl (decl-spec-list (typedef) (fixed-type "int"))
;;             (init-declr-list (declr (ident "a")) (declr (ident "b")))
;;             (comment " good stuff ")))
;; =>
;;   (values decl
;;           #f
;;           (decl-spec-list (typedef) (fixed-type "int")
;;           ((declr (ident "a")) (declr (ident "b")))
;;           ((comment " good stuff ")))
;; @end example
;; @end deffn
(define (split-decl decl)
  (let* ((tag (sx-tag decl))
	 (attr (sx-attr decl))
	 (spec-l (sx-ref decl 1))	  ; (decl-spec-list ...)
	 (dclr-l (sx-ref decl 2))	  ; (init-declr-list (...))
	 (declrs (and=> dclr-l sx-tail))) ; ((...))
    (values tag attr spec-l declrs)))

;; @deffn {Procedure} split-udecl decl => tag attr decl declr
;; This routine splits a unitized declaration into its constituent parts.
;; Get @code{(values tag attr spec-l declrs tail)}.
;; @example
;;   (split-udecl
;;      '(udecl (decl-spec-list (typedef) (fixed-type "int"))
;;              (declr (ident "a"))
;; =>
;;   (values decl
;;           #f
;;           (decl-spec-list (typedef) (fixed-type "int")
;;           (declr (ident "a")))
;; @end example
;; @end deffn
(define (split-udecl udecl)
  (let ((tag (sx-tag udecl))
	(attr (sx-attr udecl))
	(specl (sx-ref udecl 1))	; (decl-spec-list ...)
	(declr (sx-ref udecl 2)))	; (declr ...)|#f
    (values tag attr specl declr)))

;; @deffn {Procedure} unitize-decl decl [seed] [#:expand-enums #f] => seed
;; This is a fold iterator intended to be used by @code{c99-trans-unit->udict}.
;; It converts the multiple @code{init-declr} items in an @code{init-declr-list}
;; of a @code{decl} into an a-list of multiple pairs of name and @code{udecl}
;; trees with a single @code{init-declr} and no @code{init-declr-list}.
;; That is, a @code{decl} of the form
;; @example
;; (decl (decl-spec-list ...)
;;       (init-declr-list (init-declr (... "a")) (init-declr (... "b")) ...))
;; @end example
;; @noindent
;; is munged into list with elements
;; @example
;; ("a" . (udecl (decl-spec-list ...) (init-declr (... "a"))))
;; ("b" . (udecl (decl-spec-list ...) (init-declr (... "b"))))
;; @end example
;; Here we generate a dictionary of all declared items in a file:
;; @example
;; (let* ((sx0 (with-input-from-file src-file parse-c))
;;	  (sx1 (merge-inc-trees! sx0))
;;	  (name-dict (fold unitize-decl-1 '() (cdr sx1))))
;; @end example
;; Enum, struct and union def's have keys @code{(enum . "name")},
;; @code{(struct . "name")} and @code{(union . "name)}, respectively.
;; See @code{udict-struct-ref}, @code{udict-union-ref}, @code{udict-enum-ref}
;; and @code{udict-ref}.  This procecure is robust to already munged decls.
;; To capture enum values as globals use @code{->ddict}.
;; @*
;; Notes: Now saving attributes at top level.  Adding attributes for
;; struct and union (e.g., @code{__packed__}).  The latter is needed
;; because they appear in files under @file{/usr/include}.
;; @end deffn

(define* (unitize-decl decl #:optional (seed '()))
  
  (define* (make-udecl type-tag attr guts #:optional typename)
    (if (and attr (pair? attr))
	`(udecl (decl-spec-list (type-spec ,(cons* type-tag `(@ ,@attr) guts))))
	`(udecl (decl-spec-list (type-spec ,(cons type-tag guts))))))

  ;; update depends on whether unitize- procedures use fold or fold-right
  (define (update-left name value tag attr specl declrs seed)
    (acons name value (iter-declrs tag attr specl declrs seed)))

  (define (update-right name value tag attr specl declrs seed)
    (iter-declrs tag attr specl declrs (acons name value seed)))

  (define update update-right)

  ;;(simple-format #t "unitize-decl ~S\n" decl)
  (cond
   ((not (pair? decl))
    (throw 'nyacc-error "unitize-decl: bad arg: ~S" decl))
   ((eqv? (sx-tag decl) 'udecl)
    (acons (udecl-id decl) decl seed))

   ;; todo: think more about attributes
   ;; * specl attributes are merged into structs and unions right now.
   ;;   any others?
   ((eqv? (sx-tag decl) 'decl)
    (let*-values (((tag decl-attr specl declrs) (split-decl decl))
		  ((tag) (values 'udecl)))
      ;; TODO: for typedefs add attr (typedef "name") to associated udecls
      ;;(sferr "\nsx-match specl: attr=~S\n" attr) (pperr specl) (pperr declrs)
      (sx-match specl

	;; struct typedefs 
	((decl-spec-list
	  (@ . ,specl-attr)
	  (stor-spec (typedef))
	  (type-spec (struct-def (@ . ,attr) (ident ,name) . ,rest2) . ,rest1))
	 (update `(struct . ,name)
		 (make-udecl 'struct-def (attr-append attr specl-attr)
			     `((ident ,name) . ,rest2))
		 tag '() specl declrs seed))
	((decl-spec-list
	  (stor-spec (typedef))
	  (type-spec (struct-def . ,rest2) . ,rest1))
	 (iter-declrs tag decl-attr specl declrs seed))
	
	;; union typedefs 
	((decl-spec-list
	  (@ . ,specl-attr)
	  (stor-spec (typedef))
	  (type-spec (union-def (@ . ,attr) (ident ,name) . ,rest2) . ,rest1))
	 (update `(union . ,name)
		 (make-udecl 'union-def (attr-append attr specl-attr)
			     `((ident ,name) . ,rest2))
		 tag decl-attr specl declrs seed))
	((decl-spec-list
	  (stor-spec (typedef))
	  (type-spec (union-def . ,rest2) . ,rest1))
	 (iter-declrs tag decl-attr specl declrs seed))

	;; enum typedefs  -- todo: handle attributes
	((decl-spec-list
	  (stor-spec (typedef))
	  (type-spec (enum-def (ident ,name) . ,rest2) . ,rest1))
	 (update `(enum . ,name)
		 (make-udecl 'enum-def #f `((ident ,name) . ,rest2))
		 tag decl-attr specl declrs seed))
	((decl-spec-list
	  (stor-spec (typedef))
	  (type-spec (enum-def . ,rest2) . ,rest1))
	 (iter-declrs tag #f specl declrs 
		      (acons `(enum . "*anon*") (make-udecl 'enum-def #f rest2)
			     seed))
	 (update `(enum . "*anon*") (make-udecl 'enum-def #f rest2)
		 tag decl-attr specl declrs seed))
	
	;; structs
	((decl-spec-list
	  (@ . ,specl-attr)
	  (type-spec (struct-def (@ . ,attr) (ident ,name) . ,rest2) . ,rest1))
	 (update `(struct . ,name)
		 (make-udecl 'struct-def (attr-append attr specl-attr)
			     `((ident ,name) . ,rest2))
		 tag decl-attr specl declrs seed))
	((decl-spec-list
	  (type-spec (struct-def . ,rest2) . ,rest1))
	 (iter-declrs tag decl-attr specl declrs seed))

	;; unions
	((decl-spec-list
	  (type-spec (union-def (@ . ,aattr) (ident ,name) . ,rest2) . ,rest1))
	 (update `(union . ,name)
		 (make-udecl 'union-def aattr `((ident ,name) . ,rest2))
		 tag decl-attr specl declrs seed))
	((decl-spec-list
	  (type-spec (union-def . ,rest2) . ,rest1))
	 (iter-declrs tag decl-attr specl declrs seed))

	;; enums
	((decl-spec-list
	  (type-spec (enum-def (ident ,name) . ,rest2) . ,rest1))
	 (update `(enum . ,name)
		 (make-udecl 'enum-def #f `((ident ,name) . ,rest2))
		 tag decl-attr specl declrs seed))
	((decl-spec-list
	  (type-spec (enum-def . ,rest2) . ,rest1))
	 (update `(enum . "*anon*") (make-udecl 'enum-def #f rest2)
		 tag decl-attr specl declrs seed))

	(else (iter-declrs tag decl-attr specl declrs seed)))))
   
   ((eqv? (sx-tag decl) 'comp-udecl) (acons (udecl-id decl) decl seed))
   ((eqv? (sx-tag decl) 'comp-decl) (unitize-comp-decl decl seed))
   ((eqv? (sx-tag decl) 'param-decl) (unitize-param-decl decl seed))
   (else seed)))

;; @deffn {Procedure} unitize-comp-decl decl [seed]
;; This will turn
;; @example
;; (comp-decl (decl-spec-list (type-spec "int"))
;;            (comp-decl-list
;;             (comp-declr (ident "a")) (comp-declr (ident "b"))))
;; @end example
;; @noindent
;; into
;; @example
;; ("a" . (comp-udecl (decl-spec-list ...) (comp-declr (ident "a"))))
;; ("b" . (comp-udecl (decl-spec-list ...) (comp-declr (ident "b"))))
;; @end example
;; @noindent
;; This is coded to be used with fold to be consistent with other unitize
;; functions @code{struct} and @code{union} field lists.  The result needs
;; to be reversed.
;; @end deffn
(define* (unitize-comp-decl decl #:optional (seed '()))
  (cond
   ((not (pair? decl))
    (throw 'nyacc-error "unitize-decl: bad arg: ~S" decl))
   ((eqv? (sx-tag decl) 'comp-udecl)
    (acons (udecl-id decl) decl seed))
   ((eqv? (sx-tag (sx-ref decl 2)) 'comp-declr-list)
    (let-values (((tag attr spec-l declrs) (split-decl decl)))
      (iter-declrs 'comp-udecl attr spec-l declrs seed)))
   (else
    seed)))

;; @deffn {Procedure} unitize-param-decl param-decl [seed] [#:expand-enums #f]
;; This will turn
;; @example
;; (param-decl (decl-spec-list (type-spec "int"))
;;             (param-declr (ident "a")))
;; @end example
;; @noindent
;; into
;; @example
;; ("a" . (param-decl (decl-spec-list ...) (param-declr (ident "a"))))
;; @end example
;; @noindent
;; This is coded to be used with fold-right in order to preserve order
;; in @code{struct} and @code{union} field lists.
;; @*
;; TODO: What about abstract declarators?  Should use "*anon*".
;; @end deffn
(define* (unitize-param-decl decl #:optional (seed '()) #:key (expand-enums #f))
  (if (not (eqv? 'param-decl (car decl))) seed
      (let* ((tag (sx-ref decl 0))
	     (attr (sx-attr decl))
	     (spec (sx-ref decl 1))	; (type-spec ...)
	     (declr (sx-ref decl 2))	; (param-declr ...)
	     (ident (declr-ident declr))
	     (name (cadr ident)))
	(acons name decl seed))))

;; @deffn {Procedure} declr-ident declr => (ident "name")
;; Given a declarator, aka @code{init-declr}, return the identifier.
;; This is used by @code{trans-unit->udict}.
;; @end deffn
(define (declr-ident declr)
  (sx-match declr
    ((ident ,name) declr)
    ((init-declr ,declr . ,rest) (declr-ident declr))
    ((comp-declr ,declr) (declr-ident declr))
    ((param-declr ,declr) (declr-ident declr))
    ((array-of ,dir-declr ,array-spec) (declr-ident dir-declr))
    ((array-of ,dir-declr) (declr-ident dir-declr))
    ((ptr-declr ,pointer ,dir-declr) (declr-ident dir-declr))
    ((ftn-declr ,dir-declr . ,rest) (declr-ident dir-declr))
    ((scope ,declr) (declr-ident declr))
    ((bit-field ,ident . ,rest) ident)
    (else (throw 'c99-error "c99/munge: unknown declarator: ~S" declr))))

;; @deffn {Procedure} declr-id decl => "name"
;; This extracts the name from the return value of @code{declr-ident}.
;; @end deffn
(define (declr-id declr)
  (and=> (declr-ident declr) cadr))

;; @deffn {Procedure} udecl-id udecl => string
;; generate the name 
;; @end deffn
(define (udecl-id udecl)
  ;; must be udecl w/ name
  (declr-id (sx-ref udecl 2)))

;; like member but returns first non-declr of type in dict
(define (non-declr type udict)
  (let loop ((dict udict))
    (cond ((null? dict) '())
	  ((and (pair? (caar dict)) (eqv? type (caaar dict))) dict)
	  (else (loop (cdr dict))))))

(define (enum-decl-val enum-udecl name)
  ;; (decl (decl-spec-list (type-sped (enum-def (enum-def-list ...) => "123"
  (enum-ref (cadadr (cadadr enum-udecl)) name))

;; @deffn {Procedure} udict-ref name
;; @deffnx {Procedure} udict-struct-ref name
;; @deffnx {Procedure} udict-union-ref name
;; @deffnx {Procedure} udict-enum-ref name
;; @deffnx {Procedure} udict-enum-val name
;; Look up refernce in a u-dict.  If the reference is found return the
;; u-decl.  In the case of @code{udict-enum-val} the string value is returned.
;; @end deffn
(define (udict-ref udict name)
  (or (assoc-ref udict name)
      (let loop ((dict (non-declr 'enum udict)))
	(cond
	 ((null? dict) #f)
	 ((enum-decl-val (cdar dict) name) =>
	  (lambda (val) (gen-enum-udecl name val)))
	 (else (loop (non-declr 'enum (cdr dict))))))))
(define (udict-struct-ref udict name)
  (assoc-ref udict `(struct . ,name)))
(define (udict-union-ref udict name)
  (assoc-ref udict `(union . ,name)))
(define* (udict-enum-ref udict name)
  (assoc-ref udict `(enum . ,name)))
(define* (udict-enum-val udict name)
  (let loop ((dict (non-declr 'enum udict)))
    (cond ((null? dict) #f)
	  ((enum-decl-val (cdar dict) name))
	  (else (loop (non-declr 'enum (cdr dict)))))))

;; @deffn {Variable} fixed-width-int-names
;; This is a list of standard integer names (e.g., @code{"uint8_t"}).
;; @end deffn
(define fixed-width-int-names
  '("int8_t" "uint8_t" "int16_t" "uint16_t"
    "int32_t" "uint32_t" "int64_t" "uint64_t"))

;; @deffn {Procedure} typedef-decl? decl)
;; @end deffn
(define (typedef-decl? decl)
  (sx-match decl
    ((decl (decl-spec-list (stor-spec (typedef)) . ,r1) . ,r2) #t)
    (else #f)))

;; @deffn {Procedure} repl-typespec decl-spec-list repl-type-spec
;; In the decl-spec-list replace the type-specifier.
;; @end deffn
(define (repl-typespec decl-spec-list replacement)
  (fold-right
   (lambda (item seed)
     (cond ((symbol? item) (cons item seed))
	   ((eq? 'type-spec (car item)) (cons replacement seed))
	   (else (cons item seed))))
   '() decl-spec-list))

;; @deffn {Procedure} declr-list? declr
;; Determine if declr it is a list or not.
;; Often declr can be xxxx-declr-list or xxxx-declr.
;; @end deffn
(define (declr-list? declr)
  (member (sx-tag declr) '(init-declr-list comp-declr-list)))

;; === typedef expansion ===============

;; allows only one storage specifier besides typedef
;; call this (injest-in-specl orig-specl repl-specl)
(define (tdef-splice-specl orig-specl repl-specl)
  (let loop ((specl '()) (repll '()) (origl (cdr orig-specl)))
    (cond
     ((pair? repll)
      (cond
       ((equal? (car repll) '(stor-spec (typedef)))
	(loop specl (cdr repll) origl))
       ((equal? (car repll) '(stor-spec (const)))
	(loop (cons (car repll) specl) (cdr repll) origl))
       ((member (car repll) specl)	; don't duplicate other stor-spec's
	(loop specl (cdr repll) origl))
       (else 
	(loop (cons (car repll) specl) (cdr repll) origl))))
     ((pair? origl)
      (cond
       ((pmatch (car origl) ((type-spec (typename ,name)) #t) (,othersize #f))
	(loop specl (cdr repl-specl) (cdr origl))) ; now insert replacement
       ((equal? (car origl) '(stor-spec (const)))
	(loop (cons (car repll) specl) repll (cdr origl)))
       ((member (car origl) specl) ; don't duplicate "auto", "extern"
	(loop specl repll (cdr origl)))
       (else
	(loop (cons (car origl) specl) repll (cdr origl)))))
     (else
      (cons 'decl-spec-list (reverse specl))))))

;; Consider
;; @example
;; typedef int *foo_t;
;; foo_t bla[3];
;; @end example
;; @noindent
;; maps
;; @example
;; bla[3] => *(bla[3])
;; @end example
(define (tdef-splice-declr orig-declr tdef-declr)
  (define (probe-declr declr)
    (sx-match declr
      ((ident ,name)
       (sx-ref orig-declr 1))
      ((init-declr ,declr . ,rest)
       `(init-declr ,(probe-declr declr) . ,rest))
      ((comp-declr ,declr)
       `(comp-declr ,(probe-declr declr)))
      ((param-declr ,declr)
       `(param-declr ,(probe-declr declr)))
      ((array-of ,dir-declr ,array-spec)
       `(array-of ,(probe-declr dir-declr) ,array-spec))
      ((array-of ,dir-declr)
       `(array-of ,(probe-declr dir-declr)))
      ((ptr-declr ,pointer ,dir-declr)
       `(ptr-declr ,pointer ,(probe-declr dir-declr)))
      ((ftn-declr ,dir-declr . ,rest)
       `(ftn-declr ,(probe-declr dir-declr) . ,rest))
      ((scope ,declr)
       `(scope ,(probe-declr declr)))
      (else (throw 'c99-error "c99/munge: unknown declarator: ~S" declr))))
  (probe-declr tdef-declr))

;; @deffn {Procedure} tdef-splice-declr-list orig-declr-list tdef-declr
;; iterate tdef-splice-declr over a declr-init-list (or equiv)
;; @end deffn
(define (tdef-splice-declr-list orig-declr-list tdef-declr)
  (sx-cons*
   (sx-tag orig-declr-list)
   (sx-attr orig-declr-list)
   (fold-right
    (lambda (declr seed)
      (cons (tdef-splice-declr declr tdef-declr) seed))
    '() (sx-tail orig-declr-list 1))))

;; @deffn {Procedure} compound-key type-spec-tag name
;; type-spec-tag is struct/union-ref/def
;; @example
;; (compound-key 'struct-ref) => struct
;; (compount-key 'union-ref "_foo") => (union . "_foo")
;; @end example
;; @end deffn
(define* (compound-key type-spec-tag #:optional name)
  (let ((key (case type-spec-tag
	       ((struct-ref struct-def) 'struct)
	       ((union-ref union-def) 'union))))
    (if name (cons key name) key)))

;; Replace the type-spec in @var{decl-spec-list} with @var{type-spec}.
(define (replace-type-spec decl-spec-list type-spec)
  (sx-cons*
   (sx-tag decl-spec-list)
   (sx-attr decl-spec-list)
   (map
    (lambda (elt) (if (eq? (sx-tag elt) 'type-spec) type-spec elt))
    (sx-tail decl-spec-list 1))))

;; declr can be xxxx-declr-list or xxxx-declr
;; This needs to be able to accept @code{#f} @var{declr}. <= done, methinks
(define (expand-specl-typerefs specl declr udict keep)
  ;; In the process of expanding typerefs it is crutial that routines which
  ;; expand parts return the original if no change made.  That is, if there
  ;; are no changes then @code{(eq? (expand-typerefs expr) expr)} is true.
  ;; If not, then infinite loop will result.

  (define (re-expand specl declr) ;; applied after typename
    (expand-specl-typerefs specl declr udict keep))

  (define (splice-typename specl declr name udict)
    (let* ((decl (or (assoc-ref udict name) ; decl for typename
		     (throw 'c99-error "typedef not found for: ~S" name)))
	   (tdef-specl (sx-ref decl 1))	 ; specs for typename
	   (tdef-declr (sx-ref decl 2))) ; declr for typename
      (values ;; fixdd-specl fixed-declr
       (tdef-splice-specl specl tdef-specl)
       (cond ;; #f, init-declr-list, init-declr|comp-declr
	((not declr) declr)
	((declr-list? declr) (tdef-splice-declr-list declr tdef-declr))
	(else (tdef-splice-declr declr tdef-declr))))))

  (let* ((tspec (and=> (sx-find 'type-spec specl) cadr))
	 (class (sx-tag tspec))		; e.g., typename, fixed-type
	 (name (sx-ref tspec 1)))	; e.g., "foo_t"
    (case class
      ((typename)
       (cond
	((member name keep)		; keeper; don't expand
	 (values specl declr))
	((and #t ;; (pointer-declr? declr)
	      (member (cons 'pointer name) keep))
	 (values specl declr))
	(#f ;;(pointer-declr? name)		; replace with void*
	 (let ((specl (replace-type-spec specl '(type-spec (void)))))
	   (call-with-values
	       (lambda () (splice-typename specl declr name udict))
	     (lambda (specl declr) (re-expand specl declr)))))
	(else				; expand
	 (call-with-values
	     (lambda () (splice-typename specl declr name udict))
	   (lambda (specl declr) (re-expand specl declr))))))
      ((struct-def union-def)
       (let* ((tag (sx-tag tspec))
	      (attr (sx-attr tspec))
	      (fld1 (sx-ref tspec 1))
	      (ident (if (eq? 'ident (sx-tag fld1)) fld1 #f))
	      (field-list (if ident (sx-ref tspec 2) fld1))
	      (field-list (clean-field-list field-list)) ; why remove comments?
	      (orig-flds (sx-tail field-list 1))
	      (fixd-flds (map
			  (lambda (fld) (expand-typerefs fld udict keep))
			  orig-flds))
	      (fixd-field-list `(field-list ,@fixd-flds))
	      (fixd-struct (if ident
			       (sx-list tag attr ident fixd-field-list)
			       (sx-list tag attr fixd-field-list)))
	      (fixd-tspec `(type-spec ,fixd-struct))
	      (fixd-specl (replace-type-spec specl fixd-tspec)))
	 (values fixd-specl declr)))
      ((struct-ref union-ref) ;; compound reference; replace unless pointer
       (let* ((c-name (and=> (sx-find 'ident tspec)
			     (lambda (id) (sx-ref id 1))))
	      (c-key (compound-key class c-name)) ;; e.g., (struct . "foo")
	      (c-decl (and c-key (assoc-ref udict c-key)))
	      (t-spec (and c-decl (sx-find 'type-spec (sx-ref c-decl 1)))))
	 (if (or (and c-key
		      (or (member c-key keep)
			  (member (cons 'pointer c-key) keep)))
		 (not c-decl)
		 (pointer-declr? declr))
	     (values specl declr)
	     (let ((r-specl (replace-type-spec specl t-spec)))
	       (re-expand r-specl declr)))))
      ((enum-ref enum-def)
       ;; If not keeper, then replace enum with int.
       (let* ((type (sx-ref (sx-find 'type-spec specl) 1))
	      (name (and=> (sx-find 'ident type) cadr)))
	 (cond
	  ((and name (member `(enum . ,name) keep))
	   (values specl declr))
	  (else
	   (values
	    (repl-typespec specl `(type-spec (fixed-type "int")))
	    declr)))))
      (else (values specl declr)))))

;; @deffn {Procedure} expand-typerefs adecl udict [keep]
;; Given a declaration or component-declaration, return a udecl with all
;; typenames (not in the list @var{keep}), structs and unions, expanded,
;; and enums turned into int.
;; @example
;; typedef const int  (*foo_t)(int a, double b);
;; extern    foo_t    fctns[2];
;; =>
;; extern const int  (*fctns[2])(int a, double b);
;; @end example
;; @noindent
;; Note: @var{keep} was formally keyword argument.@*
;; Note: works with @code{(struct . "foo")}@*
;; Note: works with @code{(pointer . "foo_t")}
;; @end deffn
;; idea: if we have a pointer to an undefined type, then use void*
;; @*BUG HERE? if we run into a struct then the struct members have not
;; been munged into udecls.  The behavior is actually NOT DEFINED.
;; @end deffn
(define* (expand-typerefs adecl udict #:optional (keep '()))
  ;; In the process of expanding typerefs it is crutial that routines which
  ;; expand parts return the original if no change made.  That is, if there
  ;; are no changes then @code{(eq? (expand-typerefs expr) expr)} is true.
  ;; If not, then infinite loop will result.

  (define (fix-param-list param-list)
    (let* ((tail (sx-tail param-list 1))
	   (xtail
	    (let loop ((xparams '()) (chg? #f) (params tail))
	      (if (null? params)
		  (if chg? (reverse xparams) tail)
		  (case (sx-tag (car params))
		    ((param-decl)
		     (let* ((param (car params))
			    (xparam (expand-typerefs param udict keep)))
		       (if (eq? param xparam)
			   (loop (cons param xparams) chg? (cdr params))
			   (loop (cons xparam xparams) #t (cdr params)))))
		    ((ellipsis)
		     (loop (cons (car params) xparams) chg? (cdr params))))))))
      (if (eq? xtail tail)
	  param-list
	  (sx-cons* (sx-tag param-list) (sx-attr param-list) xtail))))

  ;; This will check for function declrs and fix parameters.
  (define (fix-declr declr)
    ;;(sferr "fix-declr:\n") (pperr declr)
    (and
     declr
     (sx-match declr
       ((ident ,name) declr)
       ((bit-field . ,rest) declr)
       
       ((init-declr ,declr1 . ,rest)
	(let ((xdeclr (fix-declr declr1)))
	  (if (eq? xdeclr declr1) declr `(init-declr ,xdeclr . ,rest))))
       ((comp-declr ,declr1)
	(let ((xdeclr (fix-declr declr1)))
	  (if (eq? xdeclr declr1) declr `(comp-declr ,xdeclr))))
       ((param-declr ,declr1)
	(let ((xdeclr (fix-declr declr1)))
	  (if (eq? xdeclr declr1) declr `(param-declr ,xdeclr))))
       ((array-of ,declr1 ,array-spec)
	(let ((xdeclr (fix-declr declr1)))
	  (if (eq? xdeclr declr1) declr `(array-of ,xdeclr ,array-spec))))
       ((array-of ,dir-declr)
	(let ((xdeclr (fix-declr dir-declr)))
	  (if (eq? xdeclr dir-declr) declr `(array-of ,xdeclr))))
       ((ptr-declr ,pointer ,dir-declr)
	(let ((xdeclr (fix-declr dir-declr)))
	  (if (eq? xdeclr dir-declr) declr `(ptr-declr ,pointer ,xdeclr))))

       ((scope ,declr1)
	(let ((xdeclr (fix-declr declr1)))
	  (if (eq? xdeclr declr1) declr `(scope ,xdeclr))))

       ;; abstract declarator and direct abstract declarator
       ((abs-declr ,pointer ,dir-abs-declr)
	(let ((xdeclr (fix-declr dir-abs-declr)))
	  (if (eq? xdeclr dir-abs-declr) declr `(abs-declr ,pointer ,xdeclr))))
       ((abs-declr (pointer))
	declr)
       ((abs-declr (pointer ,pointer-val))
	declr)
       ((abs-declr ,dir-abs-declr)
	(let ((xdeclr (fix-declr dir-abs-declr)))
	  (if (eq? xdeclr dir-abs-declr) declr `(abs-declr ,xdeclr))))

       ;; declr-scope
       ;; declr-array dir-abs-declr
       ;; declr-array dir-abs-declr assn-expr
       ;; declr-array dir-abs-declr type-qual-list
       ;; declr-array dir-abs-declr type-qual-list assn-expr
       ((declr-scope ,abs-declr)		; ( abs-declr )
	(let ((xdeclr (fix-declr abs-declr)))
	  (if (eq? xdeclr abs-declr) declr `(declr-scope ,xdeclr))))
       ((declr-array ,dir-abs-declr)	; []
	(let ((xdeclr (fix-declr dir-abs-declr)))
	  (if (eq? xdeclr dir-abs-declr) declr `(declr-array ,xdeclr))))
       ((declr-array ,dir-abs-declr (type-qual-list . ,type-quals))
	(let ((xdeclr (fix-declr dir-abs-declr)))
	  (if (eq? xdeclr dir-abs-declr) declr
	      `(declr-array ,xdeclr (type-qual-list . ,type-quals)))))
       ((declr-array ,dir-abs-declr ,assn-expr)
	(let ((xdeclr (fix-declr dir-abs-declr)))
	  (if (eq? xdeclr dir-abs-declr) declr
	      `(declr-array ,xdeclr ,assn-expr))))
       ((declr-array ,dir-abs-declr ,type-qual-list ,assn-expr)
	(let ((xdeclr (fix-declr dir-abs-declr)))
	  (if (eq? xdeclr dir-abs-declr) declr
	      `(declr-array ,xdeclr ,type-qual-list ,assn-expr))))
       
       ;; declr-anon-array type-qual-list assn-expr
       ((declr-anon-array ,type-qual-list ,assn-expr) declr)
       ;; declr-anon-array type-qual-list 
       ((declr-anon-array (type-qual-list ,type-qual-tail)) declr)
       ;; ???? declr-anon-array assn-expr
       ((declr-anon-array ,assn-expr) declr)
       ;; declr-anon-array
       ((declr-anon-array) declr)
       ;; declr-star dir-abs-decl
       ((declr-star ,dir-abs-declr)
	(let ((xdeclr (fix-declr dir-abs-declr)))
	  (if (eq? xdeclr dir-abs-declr) declr
	      `(declr-star ,xdeclr))))
       ;; declr-star
       ((declr-star) declr)

       ;; ftn-declr
       ((ftn-declr ,dir-declr ,param-list)
	(let ((xdeclr (fix-declr dir-declr))
	      (xparam-list (fix-param-list param-list)))
	  (if (and (eq? xdeclr dir-declr) (eq? xparam-list param-list)) declr
	      `(ftn-declr ,xdeclr ,xparam-list))))
       
       ((abs-ftn-declr ,dir-declr ,param-list)
	(let ((xdeclr (fix-declr dir-declr))
	      (xparam-list (if (eq? 'param-list (sx-tag param-list))
			       (fix-param-list param-list)
			       param-list)))
	  (if (and (eq? xdeclr dir-declr) (eq? xparam-list param-list)) declr
	      `(abs-ftn-declr ,xdeclr ,xparam-list))))
       ((anon-ftn-declr ,param-list)
	(let ((xparam-list (fix-param-list param-list)))
	  (if (eq? xparam-list param-list) declr
	      `(anon-ftn-declr ,xparam-list))))

       ;; declr-lists too
       ((init-declr-list . ,declrs)
	(let ((xdeclrs (map fix-declr declrs)))
	  (if (fold (lambda (l r seed) (and (eq? l r) seed)) #t xdeclrs declrs)
	      declr
	      `(init-declr-list . ,xdeclrs))))
       ((comp-declr-list . ,declrs)
	(let ((xdeclrs (map fix-declr declrs)))
	  ;;(sferr "    declrs, xdeclrs:\n") (pperr declrs) (pperr xdeclrs)
	  (if (fold (lambda (l r seed) (and (eq? l r) seed)) #t xdeclrs declrs)
	      declr
	      `(comp-declr-list . ,xdeclrs))))
       
       (else (throw 'c99-error "c99/munge: unknown declarator: " declr)))))

  (let*-values (((tag attr orig-specl orig-declr)
		 (split-udecl adecl))
		((repl-specl repl-declr)
		 (expand-specl-typerefs orig-specl orig-declr udict keep)))
    ;;(sferr "orig-specl, orig-declr, repl-specl, repl-declr\n")
    ;;(pperr orig-specl) (pperr orig-declr)
    ;;(pperr repl-specl) (pperr repl-declr)
    (let ((repl-declr (fix-declr repl-declr)))
      (if (and (eq? orig-specl repl-specl)
	       (eq? orig-declr repl-declr))
	  adecl ;; <= unchanged; return original
	  (sx-list tag attr repl-specl repl-declr)))))

;; === enums and defines ===============

;; @deffn {Procedure} c99-trans-unit->ddict tree [seed] [#:inc-filter proc]
;; Extract the #defines from a tree as
;; @example
;;  (define (name "ABC") (repl "repl"))
;;  (define (name "MAX") (args "X" "Y") (repl "(X)..."))
;; =>
;;  (("ABC" . "repl") ("MAX" ("X" "Y") . "(X)...") ...)
;; @end example
;; @noindent
;; The entries appear in reverse order wrt how in file.
;; @*
;; New option: #:skip-fdefs to skip function defs
;; @end deffn
(define* (c99-trans-unit->ddict tree
				#:optional (ddict '())
				#:key inc-filter skip-fdefs)
  (define (can-def-stmt defn)
    (let* ((tail (sx-tail defn 1))
	   (name (car (assq-ref tail 'name)))
	   (args (assq-ref tail 'args))
	   (repl (car (assq-ref tail 'repl))))
      (cons name (if args (cons args repl) repl))))
  (define (cpp-def? tree)
    (if (and (eq? 'cpp-stmt (sx-tag tree))
	     (eq? 'define (sx-tag (sx-ref tree 1)))
	     (not (and skip-fdefs (assq 'args (sx-tail (sx-ref tree 1) 1)))))
	(can-def-stmt (sx-ref tree 1))
	#f))
  (if (pair? tree)
      (fold
       (lambda (tree ddict)
	 (cond
	  ((cpp-def? tree) =>
	   (lambda (def-stmt)
	     (cons def-stmt ddict)))
	  ((inc-keeper? tree inc-filter) =>
	   (lambda (tree)
	     (c99-trans-unit->ddict tree ddict #:inc-filter inc-filter)))
	  (else ddict)))
       ddict
       (cdr tree))
      ddict))

;; @deffn {Procedure} udict-enums->ddict udict [ddict] => defs
;; Given a udict this generates a list that looke like the internal
;; CPP define structure.  That is,
;; @example
;; (enum-def-list (enum-def (ident "ABC")) ...)
;; @end example
;; @noindent
;; to
;; @example
;; (("ABC" . "0") ...)
;; @end example
;; @end deffn
(define* (udict-enums->ddict udict #:optional (ddict '()))
  (define (gen-nvl enum-def-list ddict)
    (let ((def-list (and=> (canize-enum-def-list enum-def-list ddict udict)
			   cdr)))
    (fold (lambda (edef ddict) ;; (enum-def (ident ,name) (fixed ,val) ...
	    (acons (sx-ref* edef 1 1) (sx-ref* edef 2 1) ddict))
	  ddict def-list)))
  (fold
   (lambda (pair ddict)
     (if (and (pair? (car pair)) (eq? 'enum (caar pair)))
	 ;; (enum . ,name) ...
	 (let* ((specl (sx-ref (cdr pair) 1))
		(tspec (car (assq-ref specl 'type-spec))))
	   (if (not (eq? 'enum-def (car tspec)))
	       (throw 'nyacc-error "udict-enums->ddict: expecting enum-def"))
	   (gen-nvl (assq 'enum-def-list (cdr tspec)) ddict))
	 ;; else ...
	 ddict))
   ddict udict))


;; === enum handling ===================

;; @deffn {Procedure} canize-enum-def-list enum-def-list [ddict] [udict] \
;;      => enum-def-list
;; Fill in constants for all entries of an enum list.
;; Expects @code{(enum-def-list (...))} (i.e., not the tail).
;; All enum-defs will have the form like @code{(fixed "1")}.
;; This will perform the transformation
;; @example
;; (enum-def-list (enum-def (ident "FOO") ...))
;; => 
;; (enum-def-list (enum-def (ident "FOO") (fixed "0") ...))
;; @end example
;; @noindent
;; @end deffn
(define* (canize-enum-def-list enum-def-list #:optional (ddict '()) (udict '()))
  (define (fail ident)
    (sferr "*** failed to convert enum ~S to constant" (sx-ref ident 1)) #f)
  (let loop ((rez '()) (nxt 0) (ddict ddict) (edl (sx-tail enum-def-list 1)))
    (cond
     ((null? edl)
      (sx-cons* (sx-tag enum-def-list) (sx-attr enum-def-list) (reverse rez)))
     (else
      (sx-match (car edl)
	((enum-defn (@ . ,attr) ,ident)
	 (let ((sval (number->string nxt)))
	   (loop (cons (sx-list 'enum-defn attr ident `(fixed ,sval)) rez)
		 (1+ nxt)  (acons (sx-ref ident 1) sval ddict) (cdr edl))))
	((enum-defn ,ident)
	 (let ((sval (number->string nxt)))
	   (loop (cons (sx-list 'enum-defn #f ident `(fixed ,sval)) rez)
		 (1+ nxt)  (acons (sx-ref ident 1) sval ddict) (cdr edl))))
	((enum-defn (@ . attr) ,ident ,expr)
	 (let* ((ival (or (eval-c99-cx expr udict ddict) (fail ident) nxt))
		(sval (number->string iv)))
	   (loop (cons (sx-list 'enum-defn attr ident `(fixed ,sval)) rez)
		 (1+ ival) (acons (sx-ref ident 1) sval ddict) (cdr edl))))
	((enum-defn ,ident ,expr)
	 (let* ((ival (or (eval-c99-cx expr udict ddict) (fail ident) nxt))
		(sval (number->string ival)))
	   (loop (cons (sx-list 'enum-defn #f ident `(fixed ,sval)) rez)
		 (1+ ival) (acons (sx-ref ident 1) sval ddict) (cdr edl)))))))))

;; @deffn {Procecure} enum-ref enum-def-list name => string
;; Gets value of enum where @var{enum-def-list} looks like
;; @example
;; (enum-def-list (enum-defn (ident "ABC") (p-expr (fixed "123")) ...))
;; @end example
;; so that
;; @example
;; (enum-def-list edl "ABC") => "123"
;; @end example
(define (enum-ref enum-def-list name)
  (let loop ((el (cdr (canize-enum-def-list enum-def-list))))
    (cond
     ((null? el) #f)
     ((not (eqv? 'enum-defn (sx-tag (car el)))) (loop (cdr el)))
     ((string=? name (sx-ref* (car el) 1 1)) (sx-ref* (car el) 1 2 1))
     (else (loop (cdr el))))))

;; @deffn {Procedure} gen-enum-udecl nstr vstr => (udecl ...)
;; @example
;; (gen-enum-udecl "ABC" "123")
;; =>
;; (udecl (decl-spec-list
;;         (type-spec
;;          (enum-def
;;           (enum-def-list
;;            (enum-defn (ident "ABC") (p-expr (fixed "123")))))))))
;; @end example
;; @end deffn
(define (gen-enum-udecl nstr vstr)
  `(udecl (decl-spec-list
	   (type-spec
	    (enum-def
             (enum-def-list
	      (enum-defn (ident ,nstr) (p-expr (fixed ,vstr)))))))))

;; === stripdown =======================

;; Remove remove @emph{stor-spec} elements and attributes from a u-decl.
;; what all?
;; * attributes (incl comments)
;; * type-specifiers: register, auto
;; * declrs: pointer-to type => pointer-to (void)
;; (stor-spec "auto") => empty

;; needs to work as stream processor, like sxpath, or foldts
;; (strip-attr)
;; (stor-spec ,name) w/ name in ("auto" "extern" "register" "static" "typedef")
;; (type-spec ,type) (void) (fixed-type ,name) (float-type ,name) aggr array

(define (stripdown-specl specl)
  (cons (sx-tag specl)
	(fold-right
	 (lambda (form seed)
	   (case (sx-tag form)
	     ((type-spec) (cons form seed))
	     ((stor-spec)
	      (if (eq? 'typedef (caadr form)) (cons form seed) seed))
	     (else seed)))
	 '() (sx-tail specl))))

(define (stripdown-declr declr)
  (define (fD seed tree)
    (sx-match tree
      ((pointer ,_1 ,_2) (values '() `(pointer ,_2)))
      ((pointer (type-qual-list . ,_)) (values '() '(pointer)))
      (,_ (values '() tree))))

  (define (fU seed kseed tree)
    (cond
     ((null? seed) (reverse kseed))
     ((eqv? (sx-tag tree) 'stor-spec) seed)
     ((eqv? (sx-tag tree) 'type-qual) seed)
     (else (cons (reverse kseed) seed))))
  
  (define (fH seed tree)
    (cons tree seed))
  
  (foldts* fD fU fH '() declr))

;; @deffn {Procedure} stripdown-udecl udecl => udecl
;; This routine removes forms from a declaration that are presumably not
;; required for FFI generation.
;; See also @code{cleanup-udecl} in @file{ffi-help.scm}.
;; @example
;; =>
;; @end example
;; @noindent
;; @end deffn
(define (stripdown-udecl udecl)
  (call-with-values
      (lambda () (split-udecl udecl))
    (lambda (tag attr specl declr)
      (sx-list tag attr (stripdown-specl specl) (stripdown-declr declr)))))

;; remove type qualifiers: "const" "volatile" and "restrict"
(define (specl-tail-rem-type-qual specl-tail)
  (remove (lambda (elt) (eq? 'type-qual (car elt))) specl-tail))
(define (specl-rem-type-qual specl)
  (if (not (eq? (sx-tag specl) 'decl-spec-list))
      (throw 'nyacc-error "expecting specl"))
  (sx-cons* (sx-tag specl) (sx-attr specl)
	    (specl-tail-rem-type-qual (sx-tail specl 1))))
(define (udecl-rem-type-qual udecl)
  (let ((tag (sx-tag udecl))
	(attr (sx-attr udecl))
	(specl (sx-ref udecl 1))
	(tail (sx-tail udecl 2)))
    (sx-cons* tag attr (specl-rem-type-qual specl) tail)))

;; @deffn {Procedure} strip-decl-spec-tail dsl-tail [#:keep-const? #f]
;; Remove cruft from declaration-specifiers (tail). ??
;; @end deffn
(define* (strip-decl-spec-tail dsl-tail #:key keep-const?)
  ;;(simple-format #t "spec=tail: ~S\n" dsl-tail)
  (let loop ((dsl1 '()) (const-seen? #f) (tail dsl-tail))
    (if (null? tail)
	(reverse (if (and const-seen? keep-const?)
		     (cons '(type-qual "const") dsl1)
		     dsl1))
	(case (caar tail)
	  ((type-qual)
	   (if (string=? (cadar tail) "const")
	       (loop dsl1 #t (cdr tail))
	       (loop dsl1 const-seen? (cdr tail))))
	  ((stor-spec)
	   (loop dsl1 const-seen? (cdr tail)))
	  (else
	   (loop (cons (car tail) dsl1) const-seen? (cdr tail)))))))

;; @deffn {Procedure} clean-field-list field-list => field-list
;; @deffnx {Procedure} clean-fields fields => fields
;; Process the tagged field-list element of a struct and remove lone comments.
;; If a field following a lone comment has no code-comment, the lone comment
;; will be used.  For example,
;; @example
;;   /* foo */
;;   int x;
;; @end example
;; @noindent
;; will be treated as if it was denereed
;; @example
;;   int x; /* foo */
;; @end example
;; @noindent
;; @end deffn
(define (clean-fields fields)
  (define (str-app-rev strl) (apply string-append (reverse strl)))
  (let loop ((rz '()) (cl '()) (fl fields))
    (if (null? fl)
	(reverse rz)
	(sx-match (car fl)
	  ((comment ,text)
	   (loop rz (cons text cl) (cdr fl)))
	  (((comp-udecl comp-decl) (@ . ,attr) . ,rest)
	   (let* ((comm (assq-ref attr 'comment))
		  (decl (car fl))
		  (decl
		   (cond
		    (comm decl)
		    ((null? cl) decl)
		    (else (sx-attr-add decl 'comment (str-app-rev cl))))))
	     (loop (cons decl rz) '() (cdr fl))))
	  (,_ (throw 'nyacc-error "clean-field-list: ~S" (car fl)))))))

(define (clean-field-list field-list)
  (cons (car field-list) (clean-fields (cdr field-list))))

;; === munged specification ============

;; @deffn {Procedure} udecl->mdecl udecl [#:add-name #f]
;; @deffnx {Procedure} udecl->mdecl/comm udecl [#:def-comm ""]
;; Turn a stripped-down unit-declaration into an m-spec.  The second version
;; includes the comment. This assumes decls have been run through
;; @code{stripdown}.
;; @example
;; (decl (decl-spec-list (type-spec "double"))
;;       (init-declr-list (
;;       (comment "state vector")
;; =>
;; ("x" "state vector" (array-of 10) (float "double")
;; @end example
;; @noindent
;; The optional keyword argument @var{add-name} provides a dummy indentifier
;; to add for abstract declarators.  If an identifier is not provided, a
;; random identifier starting with @code{@} will be provided.
;; @end deffn
(define* (udecl->mdecl decl #:key add-name)

  ;; Hmm.  We convert array size back to C code (string).  Now that I am working
  ;; on constant expression eval (eval-c99-cx) maybe we should change that.
  (define (cnvt-size-expr size-spec)
    ;;(with-output-to-string (lambda () (pretty-print-c99 size-spec)))
    size-spec)

  (define (unwrap-specl specl)
    (and=> (assq-ref (sx-tail specl) 'type-spec) car))

  (define (unwrap-pointer pointer)  ;; =>list IGNORES TYPE QUALIFIERS
    ;;(sferr "unwrap-pointer ~S\n" pointer)
    (sx-match pointer
      ((pointer (type-qual-list . ,type-qual) ,pointer)
       (cons '(pointer-to) (unwrap-pointer pointer)))
      ((pointer (type-qual-list . ,type-qual)) '((pointer-to)))
      ((pointer ,pointer) (cons '(pointer-to) (unwrap-pointer pointer)))
      ((pointer) '((pointer-to)))
      (else
       (sferr "unwrap-pointer failed on:\n") (pperr pointer)
       (throw 'nyacc-error "unwrap-pointer"))))

  (define (make-abs-dummy) ;; for abstract declarator make a dummy
    (or add-name (symbol->string (gensym "@"))))
  (define (make-abs-dummy-tail)
    (list (make-abs-dummy)))
  
  (define* (unwrap-declr declr #:key (const #f))
    ;;(sferr "unwrap-declr:\n") (pperr declr #:per-line-prefix "  ")
    (sx-match declr
      ((ident ,name)
       (list name))
      ((init-declr ,item)
       (unwrap-declr item #:const const))

      ((array-of ,dir-declr ,size)
       (cons `(array-of ,(cnvt-size-expr size)) (unwrap-declr dir-declr)))
      ((array-of ,dir-declr)
       (cons `(array-of) (unwrap-declr dir-declr)))

      ((ftn-declr ,dir-declr ,param-list)
       (cons `(function-returning ,param-list) (unwrap-declr dir-declr)))
      ((abs-ftn-declr ,dir-abs-declr)
       (cons `(function-returning) (unwrap-declr dir-abs-declr)))
      ((abs-ftn-declr ,dir-abs-declr ,param-list)
       (cons `(function-returning ,param-list) (unwrap-declr dir-abs-declr)))
      ;;((anon-ftn-declr ,param-list) ???

      ((scope ,expr) (unwrap-declr expr))

      ((ptr-declr ,pointer ,dir-declr)
       (let ((res (append (unwrap-pointer pointer) (unwrap-declr dir-declr))))
	 (if const (cons '(const) res) res)))

      ;; abstract declarator and direct abstract declarator
      ((abs-declr ,pointer ,dir-abs-declr)
       (append (unwrap-pointer pointer) (unwrap-declr dir-abs-declr)))
      ((abs-declr (pointer))
       (append (unwrap-pointer (sx-ref declr 1)) (make-abs-dummy-tail)))
      ((abs-declr (pointer ,pointer-val))
       (cons* '(pointer-to) (make-abs-dummy-tail)))
      ((abs-declr ,dir-abs-declr)
       (unwrap-declr dir-abs-declr))

      ;; declr-scope
      ;; declr-array dir-abs-declr
      ;; declr-array dir-abs-declr assn-expr
      ;; declr-array dir-abs-declr type-qual-list
      ;; declr-array dir-abs-declr type-qual-list assn-expr
      ((declr-scope ,abs-declr)		; ( abs-declr )
       (unwrap-declr abs-declr))
      ((declr-array ,dir-abs-declr)	; []
       (cons '(array-of "") (make-abs-dummy-tail))) ;; ???
      ((declr-array ,dir-abs-declr (type-qual-list . ,type-quals))
       ;; TODO: deal with "const" type-qualifier
       (cons '(array-of "") (make-abs-dummy-tail)))
      ((declr-array ,dir-abs-declr ,assn-expr)
       (cons `(array-of ,assn-expr) (unwrap-declr dir-abs-declr)))
      ((declr-array ,dir-abs-declr ,type-qual-list ,assn-expr)
       ;; TODO: deal with "const" type-qualifier
       (cons `(array-of ,assn-expr) (unwrap-declr dir-abs-declr)))

      ((bit-field (ident ,name) ,size)
       (list `(bit-field ,(cnvt-size-expr size)) name))

      ((comp-declr ,item) (unwrap-declr item))
      ((param-declr ,item) (unwrap-declr item))

      (else
       (sferr "munge/unwrap-declr missed:\n")
       (pperr declr)
       (throw 'nyacc-error "c99/munge: udecl->mdecl failed")
       #f)))

  ;;(sferr "decl:\n") (pperr decl)
  (let-values (((tag attr specl declr) (split-udecl decl)))
    (let* ((tspec (sx-ref specl 1))	; type-spec
	   (const (and=> (sx-ref specl 2)	; const pointer ???
			 (lambda (sx) (equal? (sx-ref sx 1) "const"))))
	   (declr (or (sx-ref decl 2)	; param-decl -- e.g., f(int)
		      `(ident ,(make-abs-dummy))))
	   (m-specl (unwrap-specl specl))
	   (m-declr (unwrap-declr declr #:const const))
	   (m-decl (reverse (cons m-specl m-declr))))
      ;;(sferr "decl:\n") (pperr decl)
      ;;(sferr "declr:\n") (pperr declr)
      ;;(sferr "r-mdecl: ~S\n" (cons m-specl m-declr))
      m-decl)))

(define* (udecl->mdecl/comm decl #:key (def-comm ""))
  (let* ((comm (or (and=> (assq 'comment (sx-attr decl)) cadr) def-comm))
	 (spec (udecl->mdecl decl)))
    (cons* (car spec) comm (cdr spec))))

;; @deffn {Procedure} mdecl->udecl mdecl xxx
;; needed for xxx
;; @end deffn
(define* (mdecl->udecl mdecl)

  (define (make-udecl types declr)
    ;; TODO: w/ attr needed?
    `(udecl (decl-spec-list (type-spec ,types)) (init-declr ,declr)))

  (define (doit declr mdecl-tail)
    (pmatch mdecl-tail
      (((fixed-type ,name)) (make-udecl (car mdecl-tail) declr))
      (((float-type ,name)) (make-udecl (car mdecl-tail) declr))
      (((typename ,name)) (make-udecl (car mdecl-tail) declr))
      (((void)) (make-udecl (car mdecl-tail) declr))
      
      (((pointer-to) . ,rest)
       (doit `(ptr-declr (pointer) ,declr) rest))
      (((array-of ,size) . ,rest)
       (doit `(array-of ,declr ,size) rest))
      
      (,_
       (sferr "munge/mdecl->udecl missed:\n")
       (pperr mdecl-tail)
       (throw 'nyacc-error "munge/mdecl->udecl failed")
       #f)))

  (let ((name (car mdecl))
	(rest (cdr mdecl)))
    (doit `(ident ,name) rest)))

;; === deprecated ====================

(define tree->udict c99-trans-unit->udict)
(define tree->udict/deep c99-trans-unit->udict/deep)
(define unwrap-decl unitize-decl)
(define match-comp-decl unitize-comp-decl)
(define match-param-decl unitize-param-decl)
(define expand-decl-typerefs expand-typerefs)
(define declr->ident declr-ident)
(define (fix-fields flds) (cdr (clean-field-list `(field-list . ,flds))))
(define declr-is-ptr? pointer-declr?)
(define udecl->mspec udecl->mdecl)
(define udecl->mspec/comm udecl->mdecl/comm)
(define mspec->udecl mdecl->udecl)

;;@deffn {Procedure} stripdown-1 udecl decl-dict [options]=> decl
;; This is deprecated.
;; 1) remove stor-spec
;; 2) expand typenames
;; @example
;; typedef int *x_t;
;; x_t a[10];
;; (spec (typename x_t)) (init-declr (array-of 10 (ident a)))
;; (spec (typedef) (fixed-type "int")) (init-declr (pointer) (ident "x_t"))
;; =>
;; (udecl (decl-spec-list (type-spec ...) ... (type-qual "const"))
;;        (init-declr (ptr-declr (pointer ...)
;; @end example
;; @end deffn
(define* (stripdown-1 udecl decl-dict #:key (keep '()))

  ;;(define strip-list '(stor-spec type-qual comment))
  (define strip-list '(stor-spec type-qual))

  (define (fsD seed tree)
    '())

  (define (fsU seed kseed tree)
    (cond
     ((eqv? (sx-tag tree) 'stor-spec) seed)
     ((eqv? (sx-tag tree) 'type-qual) seed)
     ((null? seed) (reverse kseed))
     (else (cons (reverse kseed) seed))))
  
  (define (fsH seed tree)
    (cons tree seed))

  (let* ((xdecl (expand-decl-typerefs udecl decl-dict keep))
	 (tag (sx-tag xdecl))
	 (attr (sx-attr xdecl))
	 (specl (sx-ref xdecl 1))
	 (declr (sx-ref xdecl 2))
	 (specl1 (foldts fsD fsU fsH '() specl)))
    (list tag specl1 declr)))

(define* (stripdown udecl #:key keep-const-ptr)
  (let* (;;(speclt (sx-tail udecl))	; decl-spec-list tail
	 (xdecl udecl)
	 (tag (sx-tag xdecl))
	 (attr (sx-attr xdecl))
	 (specl (sx-ref xdecl 1))
	 (declr (sx-ref xdecl 2))
	 (s-declr (stripdown-declr declr))
	 (is-ptr? (declr-is-ptr? declr))
	 ;;
	 (s-tag (sx-tag specl))
	 (s-attr (sx-attr specl))
	 (s-tail (strip-decl-spec-tail
		  (sx-tail specl)
		  #:keep-const? (and keep-const-ptr is-ptr?)))
	 (specl (sx-cons* s-tag s-attr s-tail)))
    ;;(pretty-print declr)
    ;;(pretty-print s-declr)
    (sx-list tag attr specl s-declr)))

;; --- last line ---

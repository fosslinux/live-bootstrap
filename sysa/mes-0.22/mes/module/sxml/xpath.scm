;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright (C) 2009  Free Software Foundation, Inc.
;;;   Modified 2004 by Andy Wingo <wingo at pobox dot com>.
;;;   Written 2001 by Oleg Kiselyov <oleg at pobox dot com> SXPath.scm.
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

;;; Taken from GNU Guile

;;; (sxml xpath) -- SXPath

;;; Commentary:
;;
;;@heading SXPath: SXML Query Language
;;
;; SXPath is a query language for SXML, an instance of XML Information
;; set (Infoset) in the form of s-expressions. See @code{(sxml ssax)}
;; for the definition of SXML and more details. SXPath is also a
;; translation into Scheme of an XML Path Language,
;; @uref{http://www.w3.org/TR/xpath,XPath}. XPath and SXPath describe
;; means of selecting a set of Infoset's items or their properties.
;;
;; To facilitate queries, XPath maps the XML Infoset into an explicit
;; tree, and introduces important notions of a location path and a
;; current, context node. A location path denotes a selection of a set of
;; nodes relative to a context node. Any XPath tree has a distinguished,
;; root node -- which serves as the context node for absolute location
;; paths. Location path is recursively defined as a location step joined
;; with a location path. A location step is a simple query of the
;; database relative to a context node. A step may include expressions
;; that further filter the selected set. Each node in the resulting set
;; is used as a context node for the adjoining location path. The result
;; of the step is a union of the sets returned by the latter location
;; paths.
;;
;; The SXML representation of the XML Infoset (see SSAX.scm) is rather
;; suitable for querying as it is. Bowing to the XPath specification,
;; we will refer to SXML information items as 'Nodes':
;;@example
;; 	<Node> ::= <Element> | <attributes-coll> | <attrib>
;; 		   | "text string" | <PI>
;;@end example
;; This production can also be described as
;;@example
;;	<Node> ::= (name . <Nodeset>) | "text string"
;;@end example
;; An (ordered) set of nodes is just a list of the constituent nodes:
;;@example
;; 	<Nodeset> ::= (<Node> ...)
;;@end example
;; Nodesets, and Nodes other than text strings are both lists. A
;; <Nodeset> however is either an empty list, or a list whose head is not
;; a symbol.  A symbol at the head of a node is either an XML name (in
;; which case it's a tag of an XML element), or an administrative name
;; such as '@@'.  This uniform list representation makes processing rather
;; simple and elegant, while avoiding confusion. The multi-branch tree
;; structure formed by the mutually-recursive datatypes <Node> and
;; <Nodeset> lends itself well to processing by functional languages.
;;
;; A location path is in fact a composite query over an XPath tree or
;; its branch. A singe step is a combination of a projection, selection
;; or a transitive closure. Multiple steps are combined via join and
;; union operations. This insight allows us to @emph{elegantly}
;; implement XPath as a sequence of projection and filtering primitives
;; -- converters -- joined by @dfn{combinators}. Each converter takes a
;; node and returns a nodeset which is the result of the corresponding
;; query relative to that node. A converter can also be called on a set
;; of nodes. In that case it returns a union of the corresponding
;; queries over each node in the set. The union is easily implemented as
;; a list append operation as all nodes in a SXML tree are considered
;; distinct, by XPath conventions. We also preserve the order of the
;; members in the union. Query combinators are high-order functions:
;; they take converter(s) (which is a Node|Nodeset -> Nodeset function)
;; and compose or otherwise combine them. We will be concerned with only
;; relative location paths [XPath]: an absolute location path is a
;; relative path applied to the root node.
;;
;; Similarly to XPath, SXPath defines full and abbreviated notations
;; for location paths. In both cases, the abbreviated notation can be
;; mechanically expanded into the full form by simple rewriting
;; rules. In case of SXPath the corresponding rules are given as
;; comments to a sxpath function, below. The regression test suite at
;; the end of this file shows a representative sample of SXPaths in
;; both notations, juxtaposed with the corresponding XPath
;; expressions. Most of the samples are borrowed literally from the
;; XPath specification, while the others are adjusted for our running
;; example, tree1.
;;
;;; Code:

(define-module (sxml xpath)
  #:use-module (ice-9 pretty-print)
  #:export (nodeset? node-typeof? node-eq? node-equal? node-pos
            xpath:filter take-until take-after map-union node-reverse
            node-trace select-kids node-self node-join node-reduce
            node-or node-closure node-parent
            sxpath))

;; Upstream version:
; $Id: SXPath.scm,v 3.5 2001/01/12 23:20:35 oleg Exp oleg $

(define (nodeset? x)
  (or (and (pair? x) (not (symbol? (car x)))) (null? x)))

;-------------------------
; Basic converters and applicators
; A converter is a function
;	type Converter = Node|Nodeset -> Nodeset
; A converter can also play a role of a predicate: in that case, if a
; converter, applied to a node or a nodeset, yields a non-empty
; nodeset, the converter-predicate is deemed satisfied. Throughout
; this file a nil nodeset is equivalent to #f in denoting a failure.

; The following function implements a 'Node test' as defined in
; Sec. 2.3 of XPath document. A node test is one of the components of a
; location step. It is also a converter-predicate in SXPath.
;
; The function node-typeof? takes a type criterion and returns a function,
; which, when applied to a node, will tell if the node satisfies
; the test.
;	node-typeof? :: Crit -> Node -> Boolean
;
; The criterion 'crit' is a symbol, one of the following:
;	id		- tests if the Node has the right name (id)
;	@		- tests if the Node is an <attributes-coll>
;	*		- tests if the Node is an <Element>
;	*text*		- tests if the Node is a text node
;	*PI*		- tests if the Node is a PI node
;	*any*		- #t for any type of Node

(define (node-typeof? crit)
  (lambda (node)
    (case crit
      ((*) (and (pair? node) (not (memq (car node) '(@ *PI*)))))
      ((*any*) #t)
      ((*text*) (string? node))
      (else
       (and (pair? node) (eq? crit (car node))))
)))


; Curried equivalence converter-predicates
(define (node-eq? other)
  (lambda (node)
    (eq? other node)))

(define (node-equal? other)
  (lambda (node)
    (equal? other node)))

; node-pos:: N -> Nodeset -> Nodeset, or
; node-pos:: N -> Converter
; Select the N'th element of a Nodeset and return as a singular Nodeset;
; Return an empty nodeset if the Nth element does not exist.
; ((node-pos 1) Nodeset) selects the node at the head of the Nodeset,
; if exists; ((node-pos 2) Nodeset) selects the Node after that, if
; exists.
; N can also be a negative number: in that case the node is picked from
; the tail of the list.
; ((node-pos -1) Nodeset) selects the last node of a non-empty nodeset;
; ((node-pos -2) Nodeset) selects the last but one node, if exists.

(define (node-pos n)
  (lambda (nodeset)
    (cond
     ((not (nodeset? nodeset)) '())
     ((null? nodeset) nodeset)
     ((eqv? n 1) (list (car nodeset)))
     ((negative? n) ((node-pos (+ n 1 (length nodeset))) nodeset))
     (else
      (or (positive? n) (error "yikes!"))
      ((node-pos (1- n)) (cdr nodeset))))))

; xpath:filter:: Converter -> Converter
; A xpath:filter applicator, which introduces a xpath:filtering context. The argument
; converter is considered a predicate, with either #f or nil result meaning
; failure.
(define (xpath:filter pred?)
  (lambda (lst)	; a nodeset or a node (will be converted to a singleton nset)
    (let loop ((lst (if (nodeset? lst) lst (list lst))) (res '()))
      (if (null? lst)
	  (reverse res)
	  (let ((pred-result (pred? (car lst))))
	    (loop (cdr lst)
		  (if (and pred-result (not (null? pred-result)))
		      (cons (car lst) res)
		      res)))))))

; take-until:: Converter -> Converter, or
; take-until:: Pred -> Node|Nodeset -> Nodeset
; Given a converter-predicate and a nodeset, apply the predicate to
; each element of the nodeset, until the predicate yields anything but #f or
; nil. Return the elements of the input nodeset that have been processed
; till that moment (that is, which fail the predicate).
; take-until is a variation of the xpath:filter above: take-until passes
; elements of an ordered input set till (but not including) the first
; element that satisfies the predicate.
; The nodeset returned by ((take-until (not pred)) nset) is a subset --
; to be more precise, a prefix -- of the nodeset returned by
; ((xpath:filter pred) nset)

(define (take-until pred?)
  (lambda (lst)	; a nodeset or a node (will be converted to a singleton nset)
    (let loop ((lst (if (nodeset? lst) lst (list lst))))
      (if (null? lst) lst
	  (let ((pred-result (pred? (car lst))))
	    (if (and pred-result (not (null? pred-result)))
		'()
		(cons (car lst) (loop (cdr lst)))))
	  ))))


; take-after:: Converter -> Converter, or
; take-after:: Pred -> Node|Nodeset -> Nodeset
; Given a converter-predicate and a nodeset, apply the predicate to
; each element of the nodeset, until the predicate yields anything but #f or
; nil. Return the elements of the input nodeset that have not been processed:
; that is, return the elements of the input nodeset that follow the first
; element that satisfied the predicate.
; take-after along with take-until partition an input nodeset into three
; parts: the first element that satisfies a predicate, all preceding
; elements and all following elements.

(define (take-after pred?)
  (lambda (lst)	; a nodeset or a node (will be converted to a singleton nset)
    (let loop ((lst (if (nodeset? lst) lst (list lst))))
      (if (null? lst) lst
	  (let ((pred-result (pred? (car lst))))
	    (if (and pred-result (not (null? pred-result)))
		(cdr lst)
		(loop (cdr lst))))
	  ))))

; Apply proc to each element of lst and return the list of results.
; if proc returns a nodeset, splice it into the result
;
; From another point of view, map-union is a function Converter->Converter,
; which places an argument-converter in a joining context.

(define (map-union proc lst)
  (if (null? lst) lst
      (let ((proc-res (proc (car lst))))
	((if (nodeset? proc-res) append cons)
	 proc-res (map-union proc (cdr lst))))))

; node-reverse :: Converter, or
; node-reverse:: Node|Nodeset -> Nodeset
; Reverses the order of nodes in the nodeset
; This basic converter is needed to implement a reverse document order
; (see the XPath Recommendation).
(define node-reverse
  (lambda (node-or-nodeset)
    (if (not (nodeset? node-or-nodeset)) (list node-or-nodeset)
	(reverse node-or-nodeset))))

; node-trace:: String -> Converter
; (node-trace title) is an identity converter. In addition it prints out
; a node or nodeset it is applied to, prefixed with the 'title'.
; This converter is very useful for debugging.

(define (node-trace title)
  (lambda (node-or-nodeset)
    (display "\n-->")
    (display title)
    (display " :")
    (pretty-print node-or-nodeset)
    node-or-nodeset))


;-------------------------
; Converter combinators
;
; Combinators are higher-order functions that transmogrify a converter
; or glue a sequence of converters into a single, non-trivial
; converter. The goal is to arrive at converters that correspond to
; XPath location paths.
;
; From a different point of view, a combinator is a fixed, named
; _pattern_ of applying converters. Given below is a complete set of
; such patterns that together implement XPath location path
; specification. As it turns out, all these combinators can be built
; from a small number of basic blocks: regular functional composition,
; map-union and xpath:filter applicators, and the nodeset union.



; select-kids:: Pred -> Node -> Nodeset
; Given a Node, return an (ordered) subset its children that satisfy
; the Pred (a converter, actually)
; select-kids:: Pred -> Nodeset -> Nodeset
; The same as above, but select among children of all the nodes in
; the Nodeset
;
; More succinctly, the signature of this function is
; select-kids:: Converter -> Converter

(define (select-kids test-pred?)
  (lambda (node)		; node or node-set
    (cond
     ((null? node) node)
     ((not (pair? node)) '())   ; No children
     ((symbol? (car node))
      ((xpath:filter test-pred?) (cdr node)))	; it's a single node
     (else (map-union (select-kids test-pred?) node)))))


; node-self:: Pred -> Node -> Nodeset, or
; node-self:: Converter -> Converter
; Similar to select-kids but apply to the Node itself rather
; than to its children. The resulting Nodeset will contain either one
; component, or will be empty (if the Node failed the Pred).
(define node-self xpath:filter)


; node-join:: [LocPath] -> Node|Nodeset -> Nodeset, or
; node-join:: [Converter] -> Converter
; join the sequence of location steps or paths as described
; in the title comments above.
(define (node-join . selectors)
  (lambda (nodeset)		; Nodeset or node
    (let loop ((nodeset nodeset) (selectors selectors))
      (if (null? selectors) nodeset
	  (loop
	   (if (nodeset? nodeset)
	       (map-union (car selectors) nodeset)
	       ((car selectors) nodeset))
	   (cdr selectors))))))


; node-reduce:: [LocPath] -> Node|Nodeset -> Nodeset, or
; node-reduce:: [Converter] -> Converter
; A regular functional composition of converters.
; From a different point of view,
;    ((apply node-reduce converters) nodeset)
; is equivalent to
;    (foldl apply nodeset converters)
; i.e., folding, or reducing, a list of converters with the nodeset
; as a seed.
(define (node-reduce . converters)
  (lambda (nodeset)		; Nodeset or node
    (let loop ((nodeset nodeset) (converters converters))
      (if (null? converters) nodeset
	  (loop ((car converters) nodeset) (cdr converters))))))


; node-or:: [Converter] -> Converter
; This combinator applies all converters to a given node and
; produces the union of their results.
; This combinator corresponds to a union, '|' operation for XPath
; location paths.
; (define (node-or . converters)
;   (lambda (node-or-nodeset)
;     (if (null? converters) node-or-nodeset
; 	(append
; 	 ((car converters) node-or-nodeset)
; 	 ((apply node-or (cdr converters)) node-or-nodeset)))))
; More optimal implementation follows
(define (node-or . converters)
  (lambda (node-or-nodeset)
    (let loop ((result '()) (converters converters))
      (if (null? converters) result
	  (loop (append result (or ((car converters) node-or-nodeset) '()))
		(cdr converters))))))


; node-closure:: Converter -> Converter
; Select all _descendants_ of a node that satisfy a converter-predicate.
; This combinator is similar to select-kids but applies to
; grand... children as well.
; This combinator implements the "descendant::" XPath axis
; Conceptually, this combinator can be expressed as
; (define (node-closure f)
;      (node-or
;        (select-kids f)
;	 (node-reduce (select-kids (node-typeof? '*)) (node-closure f))))
; This definition, as written, looks somewhat like a fixpoint, and it
; will run forever. It is obvious however that sooner or later
; (select-kids (node-typeof? '*)) will return an empty nodeset. At
; this point further iterations will no longer affect the result and
; can be stopped.

(define (node-closure test-pred?)
  (lambda (node)		; Nodeset or node
    (let loop ((parent node) (result '()))
      (if (null? parent) result
	  (loop ((select-kids (node-typeof? '*)) parent)
		(append result
			((select-kids test-pred?) parent)))
	  ))))

; node-parent:: RootNode -> Converter
; (node-parent rootnode) yields a converter that returns a parent of a
; node it is applied to. If applied to a nodeset, it returns the list
; of parents of nodes in the nodeset. The rootnode does not have
; to be the root node of the whole SXML tree -- it may be a root node
; of a branch of interest.
; Given the notation of Philip Wadler's paper on semantics of XSLT,
;  parent(x) = { y | y=subnode*(root), x=subnode(y) }
; Therefore, node-parent is not the fundamental converter: it can be
; expressed through the existing ones.  Yet node-parent is a rather
; convenient converter. It corresponds to a parent:: axis of SXPath.
; Note that the parent:: axis can be used with an attribute node as well!

(define (node-parent rootnode)
  (lambda (node)		; Nodeset or node
    (if (nodeset? node) (map-union (node-parent rootnode) node)
	(let ((pred
	       (node-or
		(node-reduce
		 (node-self (node-typeof? '*))
		 (select-kids (node-eq? node)))
		(node-join
		 (select-kids (node-typeof? '@))
		 (select-kids (node-eq? node))))))
	  ((node-or
	    (node-self pred)
	    (node-closure pred))
	   rootnode)))))

;-------------------------
; Evaluate an abbreviated SXPath
;	sxpath:: AbbrPath -> Converter, or
;	sxpath:: AbbrPath -> Node|Nodeset -> Nodeset
; AbbrPath is a list. It is translated to the full SXPath according
; to the following rewriting rules
; (sxpath '()) -> (node-join)
; (sxpath '(path-component ...)) ->
;		(node-join (sxpath1 path-component) (sxpath '(...)))
; (sxpath1 '//) -> (node-or
;		     (node-self (node-typeof? '*any*))
;		      (node-closure (node-typeof? '*any*)))
; (sxpath1 '(equal? x)) -> (select-kids (node-equal? x))
; (sxpath1 '(eq? x))    -> (select-kids (node-eq? x))
; (sxpath1 ?symbol)     -> (select-kids (node-typeof? ?symbol)
; (sxpath1 procedure)   -> procedure
; (sxpath1 '(?symbol ...)) -> (sxpath1 '((?symbol) ...))
; (sxpath1 '(path reducer ...)) ->
;		(node-reduce (sxpath path) (sxpathr reducer) ...)
; (sxpathr number)      -> (node-pos number)
; (sxpathr path-xpath:filter) -> (xpath:filter (sxpath path-xpath:filter))

(define (sxpath path)
  (lambda (nodeset)
    (let loop ((nodeset nodeset) (path path))
    (cond
     ((null? path) nodeset)
     ((nodeset? nodeset)
      (map-union (sxpath path) nodeset))
     ((procedure? (car path))
      (loop ((car path) nodeset) (cdr path)))
     ((eq? '// (car path))
      (loop
       ((if (nodeset? nodeset) append cons) nodeset
	((node-closure (node-typeof? '*any*)) nodeset))
       (cdr path)))
     ((symbol? (car path))
      (loop ((select-kids (node-typeof? (car path))) nodeset)
	    (cdr path)))
     ((and (pair? (car path)) (eq? 'equal? (caar path)))
      (loop ((select-kids (apply node-equal? (cdar path))) nodeset)
	    (cdr path)))
     ((and (pair? (car path)) (eq? 'eq? (caar path)))
      (loop ((select-kids (apply node-eq? (cdar path))) nodeset)
	    (cdr path)))
     ((pair? (car path))
      (let reducer ((nodeset
		     (if (symbol? (caar path))
			 ((select-kids (node-typeof? (caar path))) nodeset)
			 (loop nodeset (caar path))))
		    (reducing-path (cdar path)))
	(cond
	 ((null? reducing-path) (loop nodeset (cdr path)))
	 ((number? (car reducing-path))
	  (reducer ((node-pos (car reducing-path)) nodeset)
		   (cdr reducing-path)))
	 (else
	  (reducer ((xpath:filter (sxpath (car reducing-path))) nodeset)
		   (cdr reducing-path))))))
     (else
      (error "Invalid path step: " (car path)))))))

;;; arch-tag: c4e57abf-6b61-4612-a6aa-d1536d440774
;;; xpath.scm ends here

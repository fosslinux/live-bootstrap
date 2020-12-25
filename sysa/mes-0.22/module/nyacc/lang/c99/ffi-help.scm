;;; examples/nyacc/lang/c99/ffi-help.scm

;; Copyright (C) 2016-2019 Matthew R. Wette
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
;; along with this library; if not, see <http://www.gnu.org/licenses/>

;;; Notes:

;; @table code
;; @item mdecl->fh-wrapper
;; generates code to apply wrapper to objects returned from foreign call
;; @item mdecl->fh-unwrapper
;; generated code to apply un-wrapper to arguments for foreign call
;; @end table

;; also have in (bytestructures guile ffi)
;; bytestructure->descriptor->ffi-descriptor
;; bs:pointer->proc

;; TODOs
;; 1) add renamer
;; 2) think about cnvt-fctn that generates C code
;; 3) add code for bytestructures' bounding-struct-descriptor
;; 4) cnvt-udecl needs complete rewrite using udecl->mdecl from c99/munge
;; 6) generalize: typedef <anything> *foo_t;
;; 7) generalize: typedef <anything> foo_t[];

;; Issue:
;; So issue is when 'typedef struct ref foo_t' has no 'struct def'
;; we never define a type.  Then later we may see 'typedef foo_t bar_t'
;; We are using define-ffi-type-alias but that then generates a reference
;; to an undefined type.  Maybe for the above we should have a void
;; pseudo-type with
;; name: void
;; (unwrap-void obj) => 'void
;; (wrap 'void) (make-xxx)
;; (pointer-to obj) => <void* obj>
;; (value-at void*-object) =. void

;; For enum typedefs we are not creating types but just using wrappers.

;;; Code:

(define-module (nyacc lang c99 ffi-help)
  #:export (*ffi-help-version*
	    define-ffi-module
	    compile-ffi-file
	    load-include-file
	    fh-cnvt-udecl fh-cnvt-cdecl fh-cnvt-cdecl-str fh-scm-str->scm-exp
	    string-member-proc string-renamer
	    ;;
	    C-fun-decl->scm
	    ;;pkg-config-incs pkg-config-defs pkg-config-libs
	    ;; debugging
	    ;;ffi-symmap
	    )
  #:use-module (nyacc lang c99 cpp)
  #:use-module (nyacc lang c99 parser)
  #:use-module (nyacc lang c99 pprint)
  #:use-module (nyacc lang c99 munge)
  #:use-module (nyacc lang c99 cxeval)
  #:use-module (nyacc lang c99 util)
  #:use-module (nyacc version)
  #:use-module (nyacc lang sx-util)
  #:use-module ((nyacc lang util) #:select (cintstr->scm))
  #:use-module ((nyacc lex) #:select (cnumstr->scm))
  #:use-module ((nyacc util) #:select (ugly-print))
  #:use-module (system foreign)
  #:use-module (sxml fold)
  #:use-module (sxml match)
  #:use-module ((sxml xpath)
		#:renamer (lambda (s) (if (eq? s 'filter) 'sxml:filter s)))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-37)
  #:use-module (system base pmatch)
  #:use-module ((system base compile) #:select (compile-file))
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 pretty-print)
  #:re-export (*nyacc-version*)
  #:version (0 99 3))

(define fh-cpp-defs
  (cond
   ((string-contains %host-type "darwin")
    (remove (lambda (s) (string-contains s "_ENVIRONMENT_MAC_OS_X_VERSION"))
	    (get-gcc-cpp-defs)))
   (else (get-gcc-cpp-defs))))
    
(define fh-inc-dirs
  (append
   `(,(assq-ref %guile-build-info 'includedir) "/usr/include")
   (get-gcc-inc-dirs)))

(define fh-inc-help c99-def-help)

;; DEBUGGING
(set! fh-inc-dirs (cons "." fh-inc-dirs))

;; maybe change to a record-type
(define *options* (make-parameter '()))
(define *debug-parse* (make-parameter #f)) ; parse debug mode
(define *echo-decls* (make-parameter #f)) ; add echo-decls code for debugging

(define *prefix* (make-parameter "")) ; name prefix (e.g., prefix-syms)
(define *renamer* (make-parameter identity)) ; renamer from ffi-module

(define *mport* (make-parameter #t))	   ; output module port
(define *udict* (make-parameter '()))	   ; udecl dict
(define *ddict* (make-parameter '()))	   ; cpp-def dict
(define *tdefs* (make-parameter '()))	   ; typenames
(define *wrapped* (make-parameter '()))	; wrappers for foo_t and foo_t*
(define *defined* (make-parameter '()))	; type defined

(define *errmsgs* (make-parameter '()))	; list of warnings

(define (sfscm fmt . args)
  (apply simple-format (*mport*) fmt args))
(define* (ppscm tree #:key (per-line-prefix ""))
  (pretty-print tree (*mport*) #:per-line-prefix per-line-prefix))
(define* (upscm tree #:key (per-line-prefix ""))
  (ugly-print tree (*mport*) #:per-line-prefix per-line-prefix))
(define (c99scm tree)
  (pretty-print-c99 tree
		    (*mport*)
		    #:per-line-prefix ";; "))
(define (nlscm) (newline (*mport*)))

(define (sfout fmt . args)
  (apply simple-format #t fmt args))
(define (ppout tree)
  (pretty-print tree #:per-line-prefix "    "))
(define (nlout) (newline))
(define (sferr fmt . args)
  (apply simple-format (current-error-port) fmt args))
(define (pperr tree)
  (pretty-print tree (current-error-port) #:per-line-prefix "  "))

(define (fherr fmt . args)
  (apply throw 'ffi-help-error fmt args))

(define (fherr/once fmt . args)
  (let ((errmsgs (*errmsgs*)))
    (cond
     ((member fmt errmsgs)
      (apply throw 'ffi-help-error #f '()))
     (else
      (*errmsgs* (cons fmt errmsgs))
      (apply throw 'ffi-help-error fmt args)))))

;; === utilities

;; strings->symbol
(define (strings->symbol . string-list)
  (string->symbol (apply string-append string-list)))

;; '(abc def) => "abc-def"
(define (path->name path)
  (string-join (map symbol->string path) "-"))

;; '(abc def) => "abc/def"
(define (path->path path)
  (string-join (map symbol->string path) "/"))

(define (link-libs)
  (string->symbol (string-append (*prefix*) "-llibs")))

;; @deffn {Procedure} opts->attrs module-opts script-opts
;; The values in @var{script-opts} override @var{module-opts}.  That is,
;; if the value is a list then append, else replace.
;; @end deffn
(define (opts->attrs module-opts script-opts)
  ;; module-opts: inc-dirs pkg-config ...
  ;; script-opts: inc-dirs
  (fold-right
   (lambda (opt seed)
     (cond
      ((assq-ref seed (car opt)) =>
       (lambda (val)
	 (if (pair? val)
	     (acons (car opt) (append (cdr opt) val) seed)
	     (acons (car opt) val seed))))
      (else (cons opt seed))))
   (filter (lambda (pair) (symbol? (car pair))) module-opts)
   script-opts))

(define (opts->mopts opts) ;; module options to pass
  (filter (lambda (pair) (keyword? (car pair))) opts))

;; Run pkg-config
(define (pkg-config name . args)
  (if name
      (let* ((cmdstr (string-append "pkg-config" " "
				    (string-join args) " " name))
	     (port (open-input-pipe cmdstr))
	     (ostr (read-line port))
	     (status (close-pipe port))
	     (items (if (eof-object? ostr) '() (string-split ostr #\space))))
	(unless (zero? status) (fherr "failed: `~A'" cmdstr))
	items)
      '()))

;; use pkg-config to get a list of include dirs
;; (pkg-config-incs "cairo") => ("/opt/local/include/cairo" ...)
(define (pkg-config-incs name)
  (fold-right
   (lambda (s l)
     (cond 
      ((< (string-length s) 3) l)
      ((string=? "-I" (substring/shared s 0 2))
       (cons (substring/shared s 2) l))
      (else l)))
   '()
   (pkg-config name "--cflags")))

(define (pkg-config-defs name)
  (fold-right
   (lambda (s l)
     (cond 
      ((< (string-length s) 3) l)
      ((string=? "-D" (substring/shared s 0 2))
       (cons (substring/shared s 2) l))
      (else l)))
   '()
   (pkg-config name "--cflags")))

;; TODO:
;; 1) check if --libs provides -L that is not in ld.so dirs.
(define (pkg-config-libs name)
  (fold-right
   (lambda (s l)
     (cond 
      ((< (string-length s) 3) l)
      ((string=? "-lm" s) l) ;; workaround for ubuntu libm issue
      ((string=? "-l" (substring/shared s 0 2))
       (cons (string-append "lib" (substring/shared s 2)) l))
      (else l)))
   '()
   (pkg-config name "--libs")))

(define (resolve-attr-val val)
  (let* ((val (if (procedure? val) (val) val)))
    (cond
     ((eq? #f val) '())
     ((list? val) val)
     ((string? val) (list val))
     (else (throw 'ffi-help-error "value does not resolve to list")))))

(define (cintstr->num str)
  (and=> (cintstr->scm str) string->number))

(define (sw/* name) (string-append name "*"))
(define (sw/*-desc name) (string-append name "*-desc"))
(define (sw/& name) (string-append name "&"))
(define (sw/struct* name) (string-append "struct-" name "*"))
(define (sw/union* name) (string-append "struct-" name "*"))

;; I was using (pointer . name) in *defined* but this has issues
;; because expand-typerefs does not recognize it.  Is a change needed?
(define (w/struct name) (cons 'struct name))
(define (w/union name) (cons 'union name))
(define (w/enum name) (cons 'enum name))
;; pointers needed
(define (w/* name) (cons 'pointer name))
(define (w/struct* name) (cons 'pointer (cons 'struct name)))
(define (w/union* name) (cons 'pointer (cons 'union name)))

(define (udict->typenames udict)
  (fold
   (lambda (pair seed)
     (let ((name (car pair)) (decl (cdr pair)))
       (if (eq? 'typedef (and=> (sx-ref* decl 1 1 1) sx-tag))
	   (cons name seed)
	   seed)))
   '() udict))

(define (rename name)
  ((*renamer*) name))

;; === output scheme module header 

(define (ffimod-header path module-opts)
  (let* ((attrs (opts->attrs module-opts '()))
	 (pkg-config (assq-ref attrs 'pkg-config))
	 (libraries (resolve-attr-val (assq-ref attrs 'library)))
	 (libraries (append
		     (if pkg-config (pkg-config-libs pkg-config) '())
		     libraries))
 	 (libraries (delete "libm" libraries))) ;; workaround for ubuntu
    (sfscm ";; generated with `guild compile-ffi ~A.ffi'\n" (path->path path))
    (nlscm)
    (sfscm "(define-module ~S\n" path)
    (for-each
     (lambda (pair)
       (cond
	((eq? 'use-ffi-module (car pair))
	 (sfscm "  #:use-module ~S\n" (cdr pair)))))
     module-opts)
    ;;
    (for-each ;; output pass-through options
     (lambda (pair) (sfscm "  ~S " (car pair)) (ppscm (cdr pair)))
     (opts->mopts module-opts))
    ;;
    (sfscm "  #:use-module (system ffi-help-rt)\n")
    (sfscm "  #:use-module ((system foreign) #:prefix ffi:)\n")
    (sfscm "  #:use-module (bytestructures guile)\n")
    (sfscm "  )\n")
    ;;
    (ppscm
     `(define ,(link-libs)
	(list ,@(map (lambda (l) `(dynamic-link ,l)) (reverse libraries)))))
    (if (*echo-decls*) (sfscm "(define echo-decls #t)\n"))))

;; === type conversion ==============

;; argument and return values will be
;; @item int types
;; @item double float
;; @item enum => int
;; @item function (pointer)
;; @item void
;; @item pointer
;; @item struct
;; @item union
;; strings dealt with by user

;; determine if type is an "alias", that is same
;; typedef int foo_t => int
;; but use (define foo_t (bs:pointer int))

(define bs-typemap
  '(("void" . 'void) ("float" . float) ("double" . double)
    ("short" . short) ("short int" . short) ("signed short" . short)
    ("signed short int" . short) ("int" . int) ("signed" . int)
    ("signed int" . int) ("long" . long) ("long int" . long)
    ("signed long" . long) ("signed long int" . long) ("long long" . long)
    ("long long int" . long) ("signed long long" . long)
    ("signed long long int" . long)
    ("unsigned short int" . unsigned-short) ("unsigned short" . unsigned-short)
    ("unsigned int" . unsigned-int) ("unsigned" . unsigned-int)
    ("unsigned long int" . unsigned-long) ("unsigned long" . unsigned-long)
    ("unsigned long long int" . unsigned-long)
    ("unsigned long long" . unsigned-long)
    ("intptr_t" . intptr_t) ("uintptr_t" . uintptr_t) ("size_t" . size_t)
    ("ssize_t" . ssize_t) ("ptrdiff_t" . ptrdiff_t)
    ("int8_t" . int8) ("uint8_t" . uint8) 
    ("int16_t" . int16) ("uint16_t" . uint16) 
    ("int32_t" . int32) ("uint32_t" . uint32)
    ("int64_t" . int64) ("uint64_t" . uint64)
    ("float _Complex" . complex64) ("double _Complex" . complex128)
    ;; hacks:
    ("char" . int8) ("signed char" . int8) ("unsigned char" . uint8)
    ("wchar_t" . int) ("char16_t" . int16) ("char32_t" . int32)
    ("_Bool" . int8)))

(define bs-defined (map car bs-typemap))

(define (const-expr->number expr)
  (catch 'c99-error
    (lambda () (eval-c99-cx expr (*udict*) (*ddict*)))
    (lambda (key fmt . args)
      (apply throw 'ffi-help-error fmt args))))

;; just the type, so parent has to build the name-value pairs for
;; struct members
(define (mtail->bs-desc mdecl-tail)
  (let ((defined (*defined*))) ;; (udict (*udict*)))
    (pmatch mdecl-tail
      ;; expand typeref, use renamer,
      (((typename ,name))
       (let ((name (rename name)))
	 (or (assoc-ref bs-typemap name)
	     (string->symbol (string-append name "-desc")))))

      (((void)) ''void)
      (((fixed-type "char")) 'int)
      (((fixed-type "unsigned char")) 'unsigned-int)
      (((fixed-type ,fx-name)) (assoc-ref bs-typemap fx-name))
      (((float-type ,fl-name)) (assoc-ref bs-typemap fl-name))
      (((enum-def (ident ,ident) ,rest)) 'int)
      (((enum-def ,rest)) 'int)

      (((struct-def (ident ,struct-name) ,field-list))
       (mtail->bs-desc `((struct-def ,field-list))))
      (((struct-def ,field-list))
       `(bs:struct (list ,@(cnvt-field-list field-list))))
      (((struct-ref (ident ,struct-name)))
       (string->symbol (string-append "struct-" struct-name "-desc")))
      
      (((union-def (ident ,union-name) ,field-list))
       (mtail->bs-desc `((union-def ,field-list))))
      (((union-def ,field-list))
       (list 'bs:union `(list ,@(cnvt-field-list field-list))))
      (((union-ref (ident ,union-name)))
       (string->symbol (string-append "union-" union-name "-desc")))

      ;; POINTERS

      ;; typename use renamers, ... ???
      (((pointer-to) (typename ,name))
       (let ((name (rename name)))
	 (cond
	  ((assoc-ref bs-typemap name) =>
	   (lambda (n) `(bs:pointer ,n)))
	  ((member (w/* name) defined)
	   (strings->symbol name "*-desc"))
	  ((member name defined)
	   `(bs:pointer ,(strings->symbol name "-desc")))
	  (else
	   (strings->symbol name "*-desc")))))

      (((pointer-to) (void))
       `(bs:pointer 'void))

      (((pointer-to) (fixed-type "char"))
       `(bs:pointer int8))
      (((pointer-to) (fixed-type ,fx-name))
       `(bs:pointer ,(assoc-ref bs-typemap fx-name)))
      (((pointer-to) (float-type ,fx-name))
       `(bs:pointer ,(assoc-ref bs-typemap fx-name)))

      ;; bs does not support function pointers
      (((function-returning . ,rest) . ,rest)
       `(bs:pointer 'void))
      (((pointer-to) (function-returning . ,rest) . ,rest)
       `(bs:pointer 'void))
      (((pointer-to) (pointer-to) (function-returning . ,rest) . ,rest)
       `(bs:pointer 'void))

      (((pointer-to) (struct-ref . ,rest))
       (let () ;; TODO: check for struct-def ???
	 `(bs:pointer 'void)))

      ;; should use this more
      (((pointer-to) . ,rest)
       `(bs:pointer ,(mtail->bs-desc rest)))

      ;; In C99 array parameters are interpreted as pointers.
      (((array-of ,n) (fixed-type ,name))
       (let ((ns (const-expr->number n)))
	 (cond
	  ((string=? name "char") `(bs:vector ,ns int8))
	  ((string=? name "unsigned char") `(bs:vector ,ns uint8))
	  (else `(bs:vector ,ns ,(mtail->bs-desc `((fixed-type ,name))))))))
      (((array-of ,n) . ,rest)
       `(bs:vector ,(const-expr->number n) ,(mtail->bs-desc rest)))
      (((array-of) . ,rest)
       `(bs:pointer ,(mtail->bs-desc rest)))

      (((bit-field ,size) . ,rest)
       `(bit-field ,(const-expr->number size) ,(mtail->bs-desc rest)))

      (,otherwise
       (sferr "mtail->bs-desc missed mdecl:\n")
       (pperr mdecl-tail)
       (fherr "mtail->bs-desc failed")))))


;; --- output routines ---------------

(define (fhscm-export-def name)
  (let* ((name (rename name))
	 (st-name (if (string? name) name (symbol->string name)))
	 (sy-name (if (string? name) (string->symbol name) name))
	 (pred (string->symbol (string-append st-name "?")))
	 (make (string->symbol (string-append "make-" st-name))))
    ;;(sfscm "(export ~A ~A? make-~A)\n" name name name)
    (upscm `(export ,sy-name ,pred ,make))))

;;(define (fhscm-export-pdef name)
;;  (sfscm "(export ~A* ~A*? make-~A*)\n" name name name))

(define (fhscm-def-alias name orig)
  (let* ((name (rename name))
	 (s-name (if (string? name) (string->symbol name) name))
	 (s-orig (if (string? orig) (string->symbol orig) orig)))
    (ppscm `(define-public ,s-name ,s-orig))))

(define (fhscm-def-desc name desc)
  (let* ((name (rename name))
	 (s-name (if (string? name) (string->symbol name) name))
	 (s-desc (if (string? desc) (string->symbol desc) desc)))
    (ppscm `(define-public ,s-name ,s-desc))))

(define (fhscm-def-*desc name)
  (let ((name (rename name)))
    (sfscm "(define-public ~A* (bs:pointer ~A-desc))\n" name name)))

(define (fhscm-def-*desc/delay name)
  (let ((name (rename name)))
    (sfscm "(define-public ~A* (bs:pointer (delay ~A-desc)))\n" name name)))

(define (fhscm-def-compound name)
  (let* ((name (rename name))
	 (st-name (if (string? name) name (symbol->string name)))
	 (sy-name (if (string? name) (string->symbol name) name))
	 (desc (string->symbol (string-append st-name "-desc")))
	 (pred (string->symbol (string-append st-name "?")))
	 (make (string->symbol (string-append "make-" st-name))))
    (upscm `(define-fh-compound-type ,sy-name ,desc ,pred ,make))
    (fhscm-export-def name)))

(define (fhscm-def-pointer name)
  (let* ((name (rename name))
	 (st-name (if (string? name) name (symbol->string name)))
	 (sy-name (if (string? name) (string->symbol name) name))
	 (desc (string->symbol (string-append st-name "-desc")))
	 (pred (string->symbol (string-append st-name "?")))
	 (make (string->symbol (string-append "make-" st-name))))
    (upscm `(define-fh-pointer-type ,sy-name ,desc ,pred ,make))
    (fhscm-export-def name)))

(define (fhscm-def-pointer/delay name)
  (let ((name (rename name)))
    (sfscm "(define-fh-pointer-type ~A* ~A*-desc\n" name)
    (sfscm "  ~A*? make-~A*)\n" name name)
    (fhscm-export-def name)))

(define (fhscm-ref-deref typename)
  (let* ((typename (rename typename))
	 (type* (strings->symbol typename "*"))
	 (make* (strings->symbol "make-" typename "*"))
	 (type (strings->symbol typename))
	 (make (strings->symbol "make-" typename)))
    (ppscm `(ref<->deref! ,type* ,make* ,type ,make))))

(define (fhscm-def-function* name return params)
  (let* ((st-name (if (string? name) name (symbol->string name)))
	 (sy-name (if (string? name) (string->symbol name) name))
	 (wrap (string->symbol (string-append "fh-wrap-" st-name)))
	 (unwrap (string->symbol (string-append "unwrap-" st-name))))
    (sfscm "(define-public ~A-desc\n" name)
    (ppscm `(bs:pointer (delay (fh:function ,return (list ,@params))))
	   #:per-line-prefix "  ")
    (sfscm "  )\n")
    (ppscm `(define-fh-function*-type ,sy-name
	      ,(string->symbol (string-append name "-desc"))
	      ,(string->symbol (string-append name "?"))
	      ,(string->symbol (string-append "make-" name))))
    (fhscm-export-def name)))

(define* (fhscm-def-fixed name)
  (sfscm "(define unwrap-~A unwrap~~fixed)\n" name)
  (sfscm "(define wrap-~A identity)\n" name))

(define* (fhscm-def-float name)
  (sfscm "(define unwrap-~A unwrap~~float)\n" name)
  (sfscm "(define wrap-~A identity)\n" name))

;; --- structs and unions

;; scheme-bytestructures will be adding
;;   bounding-struct-descriptor union-desc => struct-desc

;; This routine will munge the fields and then perform typeref expansion.
;; `defined' here means has -desc (what?)
(define (expand-field-list-typerefs field-list)
  (let ((udict (*udict*)) (defined (*defined*)))
    (cons 'field-list
	  (fold-right
	   (lambda (pair seed)
	     (cons (expand-typerefs (cdr pair) udict defined) seed))
	   '() (fold-right unitize-comp-decl '() (cdr field-list))))))

;; field-list is (field-list . ,fields)
(define (cnvt-field-list field-list)

  (define (acons-defn name type seed)
    (cons (eval-string (simple-format #f "(quote `(~A ,~S))" name type)) seed))

  (define (acons-bfld name type seed)	; bit-field
    (let ((size (list-ref type 1)) (type (list-ref type 2)))
      (cons (eval-string
	     (simple-format #f "(quote `(~A ,~S ~A))" name type size)) seed)))

  ;;(sferr "\nfield-list:\n") (pperr field-list)
  (let* ((field-list (clean-field-list field-list)) ; remove lone comments
	 (uflds (fold-right unitize-comp-decl '() (cdr field-list))))
    ;;(sferr "field-list:\n") (pperr field-list)
    (let loop ((decls uflds))
      (if (null? decls) '()
	  (let* ((name (caar decls))
		 (udecl (cdar decls))
		 ;; fix the following, look at cleanup-udecl
		 (udecl (udecl-rem-type-qual udecl)) ;; remove "const" "extern"
		 (spec (udecl->mdecl/comm udecl))
		 (tail (cddr spec))
		 (type (mtail->bs-desc tail)))
	    (cond
	     ((and (pair? type) (eq? 'bit-field (car type)))
	      (acons-bfld name type (loop (cdr decls)))) ; bit-field
	     (else
	      (acons-defn name type (loop (cdr decls))))))))))

;; @deffn {Procedure} cnvt-aggr-def aggr-t typename aggr-name field-list
;; Output an aggregate definition, where
;; @var{attr-t} is a string of @code{"struct"} or @code{"union"},
;; @var{typename} is a string for the typename, or @code{#f},
;; @var{aggr-name} is a string for the struct or union name, or @code{#f},
;; and @var{field-list} is the field-list from the C syntax tree.
;; @end deffn
(define (cnvt-aggr-def aggr-t attr typename aggr-name field-list)
  (let* ((field-list (expand-field-list-typerefs field-list))
	 (sflds (cnvt-field-list field-list))
	 (aggr-s (symbol->string aggr-t))
	 (aggrname (and aggr-name (string-append aggr-s "-" aggr-name)))
	 (bs-aggr-t (string->symbol (string-append "bs:" aggr-s)))
	 (ty-desc (and typename (strings->symbol typename "-desc")))
	 (ty*-desc (and typename (strings->symbol typename "*-desc")))
	 (ag-desc (and aggrname (strings->symbol aggrname "-desc")))
	 (ag*-desc (and aggrname (strings->symbol aggrname "*-desc")))
	 (cattr (assoc-ref attr 'attributes)) ;; __attributes__
	 (packed? (and=> cattr
			 (lambda (l) (string-contains (car l) "__packed__" 0))))
	 (aligned? (and=> cattr
			 (lambda (l) (string-contains (car l) "__alignof__" 0))))
	 (bs-spec (if packed?
		      (list bs-aggr-t #t `(list ,@sflds))
		      (list bs-aggr-t `(list ,@sflds)))))
    (if aligned? (sferr "ffi-help: not processing __aligned__ in ~S\n"
			(or aggr-t typename)))
    (cond
     ((and typename aggr-name)
      ;;(sfscm ";; == ~A =>\n" typename)
      (ppscm `(define-public ,ty-desc ,bs-spec))
      (fhscm-def-compound typename)
      (ppscm `(define-public ,ty*-desc (bs:pointer ,ty-desc)))
      (fhscm-def-pointer (sw/* typename))
      (fhscm-ref-deref typename)
      ;;(sfscm ";; == ~A =>\n" aggrname)
      (ppscm `(define-public ,ag-desc ,ty-desc))
      (fhscm-def-compound aggrname)
      (ppscm `(define-public ,ag*-desc ,ty*-desc))
      (fhscm-def-pointer (sw/* aggrname))
      (fhscm-ref-deref aggrname))
     (typename
      (ppscm `(define-public ,ty-desc ,bs-spec))
      (fhscm-def-compound typename)
      (ppscm `(define-public ,ty*-desc (bs:pointer ,ty-desc)))
      (fhscm-def-pointer (sw/* typename))
      (fhscm-ref-deref typename))
     (aggr-name
      (ppscm `(define-public ,ag-desc ,bs-spec))
      (fhscm-def-compound aggrname)
      (ppscm `(define-public ,ag*-desc (bs:pointer ,ag-desc)))
      (fhscm-def-pointer (sw/* aggrname))
      (fhscm-ref-deref aggrname)))))

(define (cnvt-struct-def attr typename struct-name field-list)
  (cnvt-aggr-def 'struct attr typename struct-name field-list))

(define (cnvt-union-def attr typename union-name field-list)
  (cnvt-aggr-def 'union attr typename union-name field-list))

;; --- enums

(define (fhscm-def-enum name name-val-list)
  (sfscm "(define ~A-enum-nvl\n" name)
  (ppscm `(quote ,name-val-list) #:per-line-prefix "  ")
  (sfscm "  )\n")
  (sfscm "(define ~A-enum-vnl\n" name)
  (sfscm "  (map (lambda (pair) (cons (cdr pair) (car pair)))\n")
  (sfscm "       ~A-enum-nvl))\n" name)
  (sfscm "(define-public (unwrap-~A n)\n" name)
  (sfscm "  (cond\n")
  (sfscm "   ((symbol? n)\n")
  (sfscm "    (or (assq-ref ~A-enum-nvl n) (error \"bad arg\")))\n" name)
  (sfscm "   ((integer? n) n)\n")
  (sfscm "   (else (error \"bad arg\"))))\n")
  (sfscm "(define-public (wrap-~A v)\n" name)
  (sfscm "  (assq-ref ~A-enum-vnl v))\n" name))

(define (cnvt-enum-def typename enum-name enum-def-list)
  (let* ((name-val-l
	  (map
	   (lambda (def)
	     (let* ((n (sx-ref (sx-ref def 1) 1))
		    (x (sx-ref def 2))
		    (v (eval-c99-cx x '())))
	       (unless v
		 (throw 'ffi-help-error "unable to generate constant for ~S" n))
	       (cons (string->symbol n) v)))
	   (cdr (canize-enum-def-list enum-def-list)))))
    (cond
     ((and typename enum-name)
      (fhscm-def-enum typename name-val-l)
      (sfscm "(define-public unwrap-enum-~A unwrap-~A)\n" enum-name typename)
      (sfscm "(define-public wrap-enum-~A wrap-~A)\n" enum-name typename))
     (typename
      (fhscm-def-enum typename name-val-l))
     (enum-name
      (fhscm-def-enum (string-append "enum-" enum-name) name-val-l)))))

;; === function declarations : signatures for pointer->procedure

(define ffi-typemap
  ;; see system/foreign.scm
  '(("void" . ffi:void) ("float" . ffi:float) ("double" . ffi:double)
    ;;
    ("short" . ffi:short) ("short int" . ffi:short) ("signed short" . ffi:short)
    ("signed short int" . ffi:short) ("int" . ffi:int) ("signed" . ffi:int)
    ("signed int" . ffi:int) ("long" . ffi:long) ("long int" . ffi:long)
    ("signed long" . ffi:long) ("signed long int" . ffi:long)
    ("unsigned short int" . ffi:unsigned-short)
    ("unsigned short" . ffi:unsigned-short) ("unsigned int" . ffi:unsigned-int)
    ("unsigned" . ffi:unsigned-int) ("unsigned long int" . ffi:unsigned-long)
    ("unsigned long" . ffi:unsigned-long)
    ;;
    ("size_t" . ffi:size_t)
    ;;
    ("ssize_t" . ffi:ssize_t) ("ptrdiff_t" . ffi:ptrdiff_t)
    ("int8_t" . ffi:int8) ("uint8_t" . ffi:uint8) 
    ("int16_t" . ffi:int16) ("uint16_t" . ffi:uint16) 
    ("int32_t" . ffi:int32) ("uint32_t" . ffi:uint32) 
    ("int64_t" . ffi:int64) ("uint64_t" . ffi:uint64)
    ;; hacks
    ("intptr_t" . ffi:long) ("uintptr_t" . ffi:unsigned-long)
    ("char" . ffi:int8) ("signed char" . ffi:int8) ("unsigned char" . ffi:uint8)
    ("wchar_t" . int) ("char16_t" . int16) ("char32_t" . int32)
    ("long long" . ffi:long) ("long long int" . ffi:long)
    ("signed long long" . ffi:long) ("signed long long int" . ffi:long)
    ("unsigned long long" . ffi:unsigned-long)
    ("unsigned long long int" . ffi:unsigned-long)
    ("_Bool" . ffi:int8)))

(define ffi-defined (map car ffi-typemap))

(define ffi-symmap
  `((ffi:void . ,void) (ffi:float . ,float) (ffi:double . ,double)
    (ffi:short . ,short) (ffi:int . ,int) (ffi:long . ,long)
    (ffi:unsigned-short . ,unsigned-short) (ffi:unsigned-int . ,unsigned-int)
    (ffi:unsigned-long . ,unsigned-long) (ffi:size_t . ,size_t)
    (ffi:ssize_t . ,ssize_t) (ffi:ptrdiff_t . ,ptrdiff_t) (ffi:int8 . ,int8)
    (ffi:uint8 . ,uint8) (ffi:int16 . ,int16) (ffi:uint16 . ,uint16)
    (ffi:int32 . ,int32) (ffi:uint32 . ,uint32) (ffi:int64 . ,int64)
    (ffi:uint64 . ,uint64) (ffi-void* . *)))

(define (mtail->ffi-desc mdecl-tail)
  ;;(sferr "mdecl=~S\n" mdecl)
  (if (and (pair? mdecl-tail) (string? (car mdecl-tail))) (error "xxx"))
  (pmatch mdecl-tail
    (((pointer-to) . ,rest) 'ffi-void*)
    (((array-of) . ,rest) 'ffi-void*)
    (((array-of ,size) . ,rest) 'ffi-void*)
    (((fixed-type ,name))
     (or (assoc-ref ffi-typemap name)
	 (fherr/once "no FFI type for ~A" name)))
    (((float-type ,name))
     (or (assoc-ref ffi-typemap name)
	 (fherr/once "no FFI type for ~S" name)))
    (((typename ,name) . ,rest)
     (or (assoc-ref ffi-typemap name)
	 (fherr "no FFI type for ~S" name)))
    (((void)) 'ffi:void)
    (((enum-def . ,rest2) . ,rest1) 'ffi:int)
    (((enum-ref . ,rest2) . ,rest1) 'ffi:int)

    (((struct-def (field-list . ,fields)))
     `(list ,@(map (lambda (fld)
		     (let* ((udict (unitize-comp-decl fld))
			    (name (caar udict))
			    (udecl (cdar udict))
			    (udecl (udecl-rem-type-qual udecl))
			    (mdecl (udecl->mdecl udecl)))
		       (mtail->ffi-desc (cdr mdecl))))
		   fields)))
    (((struct-def (ident ,name) ,field-list))
     (mtail->ffi-desc `((struct-def ,field-list))))
    
    (((union-def (field-list . ,fields)))
     ;; TODO check libffi on how unions are passed and returned.
     ;; I assume here never passed as a floating point.
     ;; This should use bounding-struct-descriptor from bytestructures
     (let loop ((type #f) (size 0) (flds fields))
       (if (null? flds)
	   (case type ((double) 'ffi:uint64) ((float) 'ffi:uint32) (else type))
	   (let* ((udict (unitize-comp-decl (car flds)))
		  (udecl (cdar udict))
		  (udecl (udecl-rem-type-qual udecl))
		  (mdecl (udecl->mdecl udecl))
		  (ftype (mtail->ffi-desc (cdr mdecl)))
		  (ftval (assq-ref ffi-symmap ftype))
		  (fsize (sizeof ftval)))
	     (if (> fsize size)
		 (loop ftype fsize (cdr flds))
		 (loop type size (cdr flds)))))))
    (((union-def (ident ,name) ,field-list))
     (mtail->ffi-desc `((union-def ,field-list))))
    
    (,otherwise
     (sferr "mtail->ffi-desc missed:\n") (pperr mdecl-tail) ;;(quit)
     (error "") (fherr "mtail->ffi-desc missed: ~S" mdecl-tail))))

;; Return a mdecl for the return type.  The variable is called @code{NAME}.
(define (gen-decl-return udecl)
  (let* ((udecl1 (expand-typerefs udecl (*udict*) ffi-defined))
	 (udecl (udecl-rem-type-qual udecl1))
	 (mdecl (udecl->mdecl udecl1)))
    (mtail->ffi-desc (cdr mdecl))))

(define (gen-bs-decl-return udecl)
  (let* ((udecl1 (expand-typerefs udecl (*udict*) ffi-defined))
	 (udecl (udecl-rem-type-qual udecl1))
	 (mdecl (udecl->mdecl udecl1)))
    (mtail->bs-desc (cdr mdecl))))

(define (int->name ix)
  (simple-format #f "arg-~A" ix))

(define (gen-decl-params params)
  ;; Note that expand-typerefs will not eliminate enums or struct-refs :
  ;; mtail->ffi-desc needs to convert enum to int or void*
  (let loop ((ix 0) (params (fix-params params)))
    (cond
     ((null? params) '())
     ;;((equal? (car params) '(ellipsis)) (fherr/once "no varargs (yet)") '...)
     ((equal? (car params) '(ellipsis)) '())
     (else
      ;;(sferr "\nP: ~S\n" (car params))
      (let* ((udecl1 (expand-typerefs (car params) (*udict*) ffi-defined))
	     (udecl1 (udecl-rem-type-qual udecl1))
	     (mdecl (udecl->mdecl udecl1 #:add-name (int->name ix))))
	;;(sferr "  ~S\n" udecl1)
	(cons (mtail->ffi-desc (cdr mdecl))
	      (loop (1+ ix) (cdr params))))))))

(define (gen-bs-decl-params params)
  ;; Note that expand-typerefs will not eliminate enums or struct-refs :
  ;; mtail->ffi-desc needs to convert enum to int or void*
  (let loop ((ix 0) (params (fix-params params)))
    (cond
     ((null? params) '())
     ((equal? (car params) '(ellipsis)) '())
     (else
      (let* ((udecl1 (expand-typerefs (car params) (*udict*) ffi-defined))
	     (udecl1 (udecl-rem-type-qual udecl1))
	     (mdecl (udecl->mdecl udecl1 #:add-name (int->name ix))))
	(cons (mtail->bs-desc (cdr mdecl))
	      (loop (1+ ix) (cdr params))))))))

;; === function calls : unwrap args, call, wrap return

;; given mdecl for an exec argument give the unwrapper
(define (mdecl->fh-unwrapper mdecl)
  ;;(sferr "mdecl:\n") (pperr mdecl)
  (let ((wrapped (*wrapped*)) (defined (*defined*)))
    ;; git_reference_foreach_name_cb not preserved
    (pmatch (cdr mdecl)
      (((fixed-type ,name)) 'unwrap~fixed)
      (((float-type ,name)) 'unwrap~float)
      (((void)) #f)
      (((typename ,name))
       (cond ;; bit of a hack
	((member name '("float" "double")) 'unwrap~float)
	((member name '("float _Complex" "double _Complex")) 'unwrap~complex)
	((member name bs-defined) 'unwrap~fixed)
	((member name defined) `(fht-unwrap ,(string->symbol name)))
	((member name wrapped) (string->symbol (string-append "unwrap-" name)))
	(else #f)))
      
      (((enum-def (ident ,name) ,rest))
       (cond
	((member (w/enum name) wrapped)
	 (string->symbol (string-append "unwrap-enum-" name)))
	(else 'unwrap-enum)))
      (((enum-ref (ident ,name)))
       (cond
	((member (w/enum name) wrapped)
	 (string->symbol (string-append "unwrap-enum-" name)))
	(else 'unwrap-enum)))

      (((pointer-to) (typename ,typename))
       (cond
	;;((member typename ffi-defined) 'unwrap~pointer)
	((member (w/* typename) defined)
	 `(fht-unwrap ,(string->symbol (sw/* typename))))
	((member (w/* typename) wrapped)
	 (strings->symbol "unwrap-" typename "*"))
	((member typename defined)
	 `(fht-unwrap ,(string->symbol (sw/* typename))))
	((member (w/* typename) wrapped)
	 (strings->symbol "unwrap-" typename "*"))
	(else #f)))

      (((pointer-to) (struct-ref (ident ,struct-name) . ,rest))
       (cond
	((member (w/struct* struct-name) defined)
	 `(fht-unwrap ,(strings->symbol "struct-" struct-name "*")))
	((member (w/struct struct-name) defined)
	 `(fht-unwrap ,(strings->symbol "struct-" struct-name "*")))
	(else 'unwrap~pointer)))

      (((pointer-to) (function-returning (param-list . ,params)) . ,rest)
       (let* ((udecl (mdecl->udecl (cons "~ret" rest)))
	      (udecl (expand-typerefs udecl (*udict*) ffi-defined))
	      (mdecl (udecl->mdecl udecl))
	      (decl-return (mtail->ffi-desc (cdr mdecl)))
	      (decl-params (gen-decl-params params)))
	 ;;(sferr "FIX RET => ~S\n" mdecl)
	 (if (and (pair? decl-params) (equal? (last decl-params) '...))
	     (fherr/once "no varargs (yet)"))
	 `(make-fctn-param-unwrapper ,decl-return (list ,@decl-params))))
      
      (((pointer-to) . ,otherwise) 'unwrap~pointer)

      ;; TODO: int b[]
      ;; make ffi-help-rt unwrap bytevector  
      (((array-of ,size) . ,rest) 'unwrap~array)
      (((array-of) . ,rest) 'unwrap~array)

      (,otherwise
       (sferr "mdecl->fh-unwrapper missed:\n") (pperr mdecl) (quit)
       (fherr "mdecl->fh-unwrapper missed: ~S" mdecl)))))

(define (mdecl->fh-wrapper mdecl)
  (let ((wrapped (*wrapped*)) (defined (*defined*)))
    (pmatch (cdr mdecl)
      (((fixed-type ,name)) (if (assoc-ref ffi-typemap name) #f
				(fherr "todo: ffi-wrap fixed")))
      (((float-type ,name)) (if (assoc-ref ffi-typemap name) #f
				(fherr "todo: ffi-wrap float")))
      (((void)) #f)
      (((typename ,name))
       (cond
	((member name bs-defined) #f)
	((member name defined) `(fht-wrap ,(string->symbol name)))
	((member name wrapped) (string->symbol (string-append "wrap-" name)))
	(else #f)))

      (((enum-def (ident ,name) ,rest))
       (cond
	((member (w/enum name) wrapped)
	 (string->symbol (string-append "wrap-enum-" name)))
	(else 'wrap-enum)))
      (((enum-ref (ident ,name)))
       (cond
	((member (w/enum name) wrapped)
	 (string->symbol (string-append "wrap-enum-" name)))
	(else 'wrap-enum)))

      (((pointer-to) (typename ,typename))
       (cond
	;;??((member typename ffi-defined) 'wrap~pointer)
	((member typename defined)
	 `(fht-wrap ,(strings->symbol typename "*")))
	((member typename wrapped)
	 (strings->symbol "wrap-" typename "*"))
	(else #f)))
      
      (((pointer-to) (struct-ref (ident ,struct-name) . ,rest))
       (cond
	((member (w/struct struct-name) wrapped)
	 `(fht-wrap ,(string->symbol (sw/struct* struct-name))))
	(else #f)))

      (((pointer-to) (union-ref (ident ,union-name) . ,rest))
       (cond
	((member (w/union union-name) wrapped)
	 `(fht-wrap ,(string->symbol (sw/union* union-name))))
	(else #f)))

      ;;(((pointer-to) . ,otherwise) 'ffi:make-pointer)
      (((pointer-to) . ,otherwise) #f)

      (,otherwise (fherr "mdecl->fh-wrapper missed: ~S" mdecl)))))

;; given list of udecl params generate list of name-unwrap pairs
(define (gen-exec-params params)
  (fold-right
   (lambda (param-decl seed)
     (cond
      ;;((equal? (car params) '(ellipsis)) (fherr/once "no varargs (yet)") '...)
      ((equal? param-decl '(ellipsis)) seed)
      (else
       ;; Changed to (*wrapped*) to include enum types.  If we need (*defined*)
       ;; then we will need to create enum types in cnvt-udecl typedefs.
       (let* ((param-decl (expand-typerefs param-decl (*udict*) (*wrapped*)))
	      (param-decl (udecl-rem-type-qual param-decl)) ;; ???
	      (mdecl (udecl->mdecl param-decl)))
	 (acons (car mdecl) (mdecl->fh-unwrapper mdecl) seed)))))
   '() params))

;; given list of name-unwrap pairs generate function arg names
(define (gen-exec-arg-names params)
  (map (lambda (s) (string->symbol (car s))) params))

(define (gen-exec-unwrappers params)
  (fold-right
   (lambda (name-unwrap seed)
     (let ((name (car name-unwrap))
	   (unwrap (cdr name-unwrap)))
       (if unwrap
	   (cons `(,(string->symbol (string-append "~" name))
		   (,unwrap ,(string->symbol name)))
		 seed)
	   seed)))
   '()
   params))

;; This generates the list of arguments to the actual call.
(define (gen-exec-call-args params)
  (fold-right
   (lambda (name-unwrap seed)
     (let ((name (car name-unwrap))
	   (unwrap (cdr name-unwrap)))
       (cons (string->symbol (if unwrap (string-append "~" name) name)) seed)))
   '()
   params))

(define (gen-exec-return-wrapper udecl)
  (let* ((udecl (expand-typerefs udecl (*udict*) (*wrapped*)))
	 (udecl (udecl-rem-type-qual udecl))
	 (mdecl (udecl->mdecl udecl)))
    (mdecl->fh-wrapper mdecl)))

(define (fix-params param-decls)

  (define (remove-void-param params)
    (if (and (pair? params) (null? (cdr params))
	     (equal? (car params)
		     '(param-decl (decl-spec-list (type-spec (void))))))
	'() params))
  
  (define (fix-param param-decl ix)
    ;; should this fix param names? -- above code should deal with it
    (sxml-match param-decl
      ((param-decl (decl-spec-list . ,specl))
       `(param-decl (decl-spec-list . ,specl)
		    (init-declr (ident ,(simple-format #f "arg-~A" ix)))))
      (,otherwise param-decl)))

  (let loop ((ix 0) (decls (remove-void-param param-decls)))
    (if (null? decls) '()
	(cons (fix-param (car decls) ix) (loop (1+ ix) (cdr decls))))))

;; @deffn {Procedure} cnvt-fctn name specl params
;; name is string
;; specl is decl-spec-list tree
;; params is list of param-decl trees (i.e., cdr of param-list tree)
;; @end deffn
(define (cnvt-fctn name rdecl params)
  (let* ((params (fix-params params))
	 (varargs? (and (pair? params) (equal? (last params) '(ellipsis))))
	 (decl-return (gen-decl-return rdecl))
	 (decl-params (gen-decl-params params))
	 (exec-return (gen-exec-return-wrapper rdecl))
	 (exec-params (gen-exec-params params))
	 (sname (string->symbol name))
	 (~name (string->symbol (string-append "~" name)))
	 ;;(call `(,~name ,@(gen-exec-call-args exec-params)))
	 (va-call `(apply ,~name ,@(gen-exec-call-args exec-params)
			  (map cdr ~rest)))
	 (call `((force ,~name) ,@(gen-exec-call-args exec-params))))
    (cond
     (varargs?
      (sfscm ";; to be used with fh-cast\n")
      (ppscm
       `(define (,sname ,@(gen-exec-arg-names exec-params) . ~rest)
	  (let ((,~name (fh-link-proc
			 ,decl-return ,name
			 (append (list ,@decl-params) (map car ~rest))
			 ,(link-libs)))
		,@(gen-exec-unwrappers exec-params))
	    ,(if exec-return (list exec-return va-call) va-call)))))
     (#f ;; separate ~name and name defines
      (ppscm `(define ,~name
		(delay (fh-link-proc ,decl-return ,name (list ,@decl-params)
				     ,(link-libs)))))
      (ppscm
       `(define (,sname ,@(gen-exec-arg-names exec-params))
	  (let ,(gen-exec-unwrappers exec-params)
	    ,(if exec-return (list exec-return call) call)))))
     (else ;; combined ~name and name defines
      (ppscm
       `(define ,sname
	  (let ((,~name
		 (delay (fh-link-proc ,decl-return ,name (list ,@decl-params)
				      ,(link-libs)))))
	    (lambda ,(gen-exec-arg-names exec-params)
	      (let ,(gen-exec-unwrappers exec-params)
		,(if exec-return (list exec-return call) call))))))))
    (sfscm "(export ~A)\n" name)))

;; === externs ========================

(define (cnvt-extern name ms-tail)
  (let ((desc (mtail->bs-desc ms-tail)))
    (sfscm ";; (~A) => bytestructure\n" name)
    (ppscm
     `(define-public ,(string->symbol name)
	(let ((x-promise (delay (fh-link-extern ,name ,desc ,(link-libs)))))
	  (lambda () (force x-promise)))))))

;; ------------------------------------

;; intended to provide decl's for pointer-to or vector-of args
(define (get-needed-defns params keep-list)
  (sferr "get-needed-defns [NOT DONE]\n") (pperr params)
  '())

;; extract (struct-def ...) from (udecl ...)
(define find-struct-def
  (let ((find-proc (sxpath '(// struct-def))))
    (lambda (udecl)
      (and=> (find-proc udecl) car))))

;; extract (union-def ...) from (udecl ...)
(define find-union-def
  (let ((find-proc (sxpath '(// union-def))))
    (lambda (udecl)
      (and=> (find-proc udecl) car))))


;; assume unit-declarator
;; TODO (ptr-declr (pointer (type-qual-list (type-qual "const"))))
;; See also stripdown-specl and stripdown-declr in @file{munge.scm}.
(define (cleanup-udecl specl declr)
  (let* ((fctn? (pair? ((sxpath '(// ftn-declr)) declr)))
	 (specl (remove (lambda (node)
			  (or (equal? node '(stor-spec (auto)))
			      (equal? node '(stor-spec (register)))
			      (equal? node '(stor-spec (static)))
			      (and (pair? node)
				   (equal? (car node) 'type-qual))))
			specl))
	 (specl (if fctn?
		    (remove (lambda (node)
			      (equal? node '(stor-spec (extern)))) specl)
		    specl)))
    (values specl declr)))

;; @deffn {Procecure} back-ref-extend! decl typename
;; @deffnx {Procecure} back-ref-getall decl typename
;; The first procecure adds a backward reference for a struct from typedef
;; forward reference.  The second procedure returns the list of references.
;; This is sort of a hack but don't want to carry a list of forward
;; references just yet.
;; @end deffn
(define (back-ref-extend! decl typename)
  (let ((aval (sx-attr-ref decl 'typedef)))
    (sx-attr-set! decl 'typedef
		  (if aval (string-append aval "," typename) typename))))
(define (back-ref-getall decl)
  (let ((aval (sx-attr-ref decl 'typedef)))
    (if aval (string-split aval #\,) '())))

;;(display "TODO: struct xyz { }; followed by typedef struct xyz *xyz_t\n")
;;^-- instead have user run (ref<->deref! ...

;; @deffn {Procedure} cnvt-udecl udecl udict wrapped defined)
;; Given udecl produce a ffi-spec.
;; Return updated (string based) keep-list, which will be modified if the
;; declaration is a typedef.  The typelist is the set of keepers used for
;; @code{udecl->mdecl}.
;; Returns values wrapped, defined.
;; @end deffn
;; NOT SURE WHAT defined MEANS NOW
;; was bytestructure in fh-type, but for
;; for any type we also declare a poitner type
;; TODO: decls need to be broken out into one of the forms:
;; function typedef struct-ref/def union-ref/def enum variable
(define (cnvt-udecl udecl udict wrapped defined)
  ;; This is a bit sloppy in that we have to know if the converters are
  ;; creating wrappers and/or (type) defines.
  
  (define (ptr-decl specl)
    `(udecl ,specl (init-declr (ptr-declr (pointer) (ident "_")))))
  (define (non-ptr-decl specl)
    `(udecl ,specl (init-declr (ident "_"))))
  
  ;; use fluids OR pass around
  (*wrapped* wrapped)
  (*defined* defined)

  (let*-values (((tag attr specl declr) (split-udecl udecl))
		((specl declr) (cleanup-udecl specl declr))
		((clean-udecl) (values (sx-list tag #f specl declr))))

    (sxml-match clean-udecl

      ;; typedef void **ptr_t;
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef)) (type-spec (void)))
	(init-declr (ptr-declr (pointer (pointer)) (ident ,typename))))
       ;; FIX
       (sfscm "(define-public ~A-desc (bs:pointer (bs:pointer 'void)))\n"
	      typename)
       (fhscm-def-pointer typename)
       (values (cons typename wrapped) (cons typename defined)))
      
      ;; typedef void *ptr_t;
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef)) (type-spec (void)))
	(init-declr (ptr-declr (pointer) (ident ,typename))))
       ;; FIX
       (sfscm "(define-public ~A-desc (bs:pointer 'void))\n" typename)
       (fhscm-def-pointer typename)
       (values (cons typename wrapped) (cons typename defined)))
      
      ;; typedef void proxy_t;
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (void)))
	(init-declr (ident ,typename)))
       ;; FIX
       (sfscm "(define-public ~A-desc 'void)\n" typename)
       (sfscm "(define-public ~A*-desc (bs:pointer ~A-desc))\n"
	      typename typename)
       (fhscm-def-pointer (sw/* typename))
       (values (cons* typename (w/* typename) wrapped)
	       (cons* typename (w/* typename) defined)))
      
      ;; typedef int foo_t;
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (fixed-type ,name)))
	(init-declr (ident ,typename)))
       ;; FIX
       (sfscm "(define-public ~A-desc ~A)\n"
	      typename (assoc-ref bs-typemap name))
       (values wrapped defined))

      ;; typedef double foo_t;
      ;; If fh-object? then should be bytestructure.
      ;; Should wrap be to number or bytestructure?
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (float-type ,name)))
	(init-declr (ident ,typename)))
       (sfscm "(define-public ~A-desc ~A)\n"
	      typename (assoc-ref bs-typemap name))
       (values wrapped defined))

      ;; typedef foo_t *foo_ptr_t;
      ((udecl
	(decl-spec-list (stor-spec (typedef)) (type-spec (typename ,name)))
	(init-declr (ptr-declr (pointer) (ident ,typename))))
       (cond
	((member name defined)
	 ;; FIX
	 (sfscm "(define-public ~A-desc (bs:pointer ~A-desc))\n" typename name)
	 (fhscm-def-pointer typename))
	(else
	 ;; FIX
	 (sfscm "(define-public ~A-desc (bs:pointer 'void))\n" typename)
	 (fhscm-def-pointer typename)))
       (values (cons typename wrapped) (cons typename defined)))

      ;; typedef foo_t **foo_ptr_t;
      ((udecl
	(decl-spec-list (stor-spec (typedef)) (type-spec (typename ,name)))
	(init-declr (ptr-declr (pointer (pointer)) (ident ,typename))))
       (sfscm "(define-public ~A-desc (bs:pointer (bs:pointer ~A-desc)))\n"
	      typename name)
       (fhscm-def-pointer typename)
       (values (cons typename wrapped) (cons typename defined)))

      ;; typedef enum foo { ... } foo_t;
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (enum-def (ident ,enum-name) ,enum-def-list . ,rest)))
	(init-declr (ident ,typename)))
       (cnvt-enum-def typename enum-name enum-def-list)
       (values (cons* typename (w/enum enum-name) wrapped) defined))

      ;; typedef enum { ... } foo_t;
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (enum-def ,enum-def-list . ,rest)))
	(init-declr (ident ,typename)))
       (cnvt-enum-def typename #f enum-def-list)
       (values (cons typename wrapped) defined))

      ;; typedef enum foo foo_t;
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (enum-ref (ident ,enum-name))))
	(init-declr (ident ,typename)))
       (sfscm "(define-public wrap-~A wrap-enum-~A)\n" typename enum-name)
       (sfscm "(define-public unwrap-~A unwrap-enum-~A)\n" typename enum-name)
       (values (cons typename wrapped) defined))

      ;; need better way ???
      ;; typedef struct foo { ... } foo_t;
      ;; missing typedef struct foo { ... } *foo_t;
      ;; typedef struct { ... } foo_t;
      ;; missing typedef struct { ... } *foo_t;
      ;; typedef struct foo foo_t;
      ;; typedef struct foo *foo_t;
      ;; =>
      ;; (decl-spec-list
      ;;  (stor-spec typedef) (type-spec (struct-def . ,rest)))
      ;; (init-declr . ,rest)
      ;; 

      ;; typedef struct foo { ... } foo_t;
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (struct-def (@ . ,attr1) (ident ,struct-name) ,field-list)))
	(init-declr (ident ,typename)))
       (cnvt-struct-def attr1 typename struct-name field-list)
       (values
	;; Hoping don't need to add (w/struct* struct-name)
	(cons* typename (w/* typename) (w/struct struct-name) wrapped)
	(cons* typename (w/* typename) (w/struct struct-name) defined)))

      ;; typedef struct foo { ... } *foo_t;
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (struct-def (@ . ,attr1) (ident ,struct-name) ,field-list)))
	(init-declr (ptr-declr (pointer) (ident ,typename))))
       (cnvt-struct-def attr1 #f struct-name field-list)
       (sfscm "(define-public ~A-desc struct-~A*-desc)\n" typename struct-name)
       (fhscm-def-pointer typename)
       (values
	(cons* typename (w/* typename)
	       (w/struct struct-name) (w/struct* struct-name)
	       wrapped)
	(cons* typename (w/* typename)
	       (w/struct struct-name) (w/struct* struct-name)
	       defined)))

      ;; typedef struct { ... } foo_t;
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (struct-def (@ . ,attr1) ,field-list)))
	(init-declr (ident ,typename)))
       (cnvt-struct-def attr1 typename #f field-list)
       (values (cons* typename (w/* typename) wrapped)
	       (cons* typename (w/* typename) defined)))

      ;; typedef struct { ... } *foo_t;
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (struct-def (@ . ,attr1) ,field-list)))
	(init-declr (ptr-declr (pointer) (ident ,typename))))
       (cnvt-struct-def attr1 (sw/* typename) #f field-list)
       (values (cons* (w/* typename) wrapped)
	       (cons* (w/* typename) defined)))

      ;; typedef struct foo foo_t;
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (struct-ref (ident ,struct-name))))
	(init-declr (ident ,typename)))
       (cond
	;; This case represents three possible uses:
	((member (w/struct struct-name) defined)
	 ;; 1) struct defined previously
	 (sfscm "(define-public ~A-desc struct-~A-desc)\n" typename struct-name)
	 (sfscm "(define-public ~A*-desc struct-~A*-desc)\n"
		typename struct-name)
	 (fhscm-def-compound typename))
	((udict-struct-ref udict struct-name) =>
	 ;; 2) struct defined later
	 (lambda (struct-decl)
	   (back-ref-extend! struct-decl typename)
	   (sfscm "(define-public ~A-desc 'void)\n" typename)
	   (sfscm "(define-public ~A fh-void)\n" typename)
	   (sfscm "(define-public ~A? fh-void?)\n" typename)
	   (sfscm "(define-public make-~A make-fh-void)\n" typename)
	   (sfscm "(define-public ~A*-desc (bs:pointer (delay ~A-desc)))\n"
		  typename typename)))
	(else
	 ;; 3) struct never defined; only used as pointer
	 (sfscm "(define-public ~A-desc 'void)\n" typename)
	 (sfscm "(define-fh-type-alias ~A fh-void)\n" typename)
	 (sfscm "(define-public ~A? fh-void?)\n" typename)
	 (sfscm "(define-public make-~A make-fh-void)\n" typename)
	 (sfscm "(define-public ~A*-desc (bs:pointer ~A-desc))\n"
		typename typename)))
       (fhscm-def-pointer (sw/* typename))
       (values (cons* typename (w/* typename) wrapped)
	       (cons* typename (w/* typename) defined)))

      ;; typedef struct foo *foo_t;
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (struct-ref (ident ,struct-name))))
	(init-declr (ptr-declr (pointer) (ident ,typename))))
       (cond
	;; This case represents three possible uses:
	((member struct-name defined)
	 ;; 1) struct defined previously
	 (sfscm "(define-public ~A-desc struct-~A*-desc)\n"
		typename struct-name))
	((udict-struct-ref udict struct-name) =>
	 ;; 2) struct defined later
	 (lambda (struct-decl)
	   (back-ref-extend! struct-decl (sw/& typename))
	   (sfscm "(define-public ~A&-desc 'void)\n" typename)
	   (sfscm "(define-public ~A-desc (bs:pointer (delay ~A&-desc)))\n"
		  typename typename)))
	(else
	 ;; 3) struct never defined; only used as pointer
	 (sfscm "(define-public ~A-desc (bs:pointer 'void))\n" typename)))
       (fhscm-def-pointer typename)
       (values (cons typename wrapped) (cons typename defined)))

      ;; typedef union foo { ... } foo_t;
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (union-def (ident ,union-name) ,field-list)))
	(init-declr (ident ,typename)))
       (cnvt-union-def #f typename union-name field-list)
       (values
	(cons* typename (w/* typename) (w/union union-name) wrapped)
	(cons* typename (w/* typename) (w/union union-name) defined)))

      ;; typedef union { ... } foo_t;
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (union-def ,field-list)))
	(init-declr (ident ,typename)))
       (cnvt-union-def #f typename #f field-list)
       (values (cons* typename (w/* typename) wrapped)
	       (cons* typename (w/* typename) defined)))

      ;; typedef union foo foo_t;
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (union-ref (ident ,union-name))))
	(init-declr (ident ,typename)))
       (cond
	;; This case represents three possible uses:
	((member (w/union union-name) defined)
	 ;; 1) union defined previously
	 (sfscm "(define-public ~A-desc union-~A-desc)\n" typename union-name)
	 (sfscm "(define-public ~A*-desc union-~A*-desc)\n"
		typename union-name)
	 (fhscm-def-compound typename))
	((udict-union-ref udict union-name) =>
	 ;; 2) union defined later
	 (lambda (union-decl)
	   (back-ref-extend! union-decl typename)
	   (sfscm "(define-public ~A-desc 'void)\n" typename)
	   (sfscm "(define-public ~A fh-void)\n" typename)
	   (sfscm "(define-public ~A? fh-void?)\n" typename)
	   (sfscm "(define-public make-~A make-fh-void)\n" typename)
	   (sfscm "(define-public ~A*-desc (bs:pointer (delay ~A-desc)))\n"
		  typename typename)))
	(else
	 ;; 3) union never defined; only used as pointer
	 (sfscm "(define-public ~A-desc 'void)\n" typename)
	 (sfscm "(define-public ~A*-desc (bs:pointer ~A-desc))\n"
		typename typename)))
       (fhscm-def-pointer (sw/* typename))
       (values (cons* typename (w/* typename) wrapped)
	       (cons* typename (w/* typename) defined)))

      ;; typedef union foo *foo_t;
      ;; TODO: check for forward reference
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (union-ref (ident ,union-name))))
	(init-declr (ptr-declr (pointer) (ident ,typename))))
       (cond
	;; This case represents three possible uses:
	((member union-name defined)
	 ;; 1) union defined previously
	 (sfscm "(define-public ~A-desc union-~A*-desc)\n"
		typename union-name))
	((udict-union-ref udict union-name) =>
	 ;; 2) union defined later
	 (lambda (union-decl)
	   (back-ref-extend! union-decl (sw/& typename))
	   (sfscm "(define-public ~A&-desc 'void)\n" typename)
	   (sfscm "(define-public ~A-desc (bs:pointer (delay ~A&-desc)))\n"
		  typename typename)))
	(else
	 ;; 3) union never defined; only used as pointer
	 (sfscm "(define-public ~A-desc (bs:pointer 'void))\n" typename)))
       (fhscm-def-pointer typename)
       (values (cons typename wrapped) (cons typename defined)))

      ;; typedef int (*foo_t)(int x, ...);
      ;; extern int git_reference_foreach(git_repository *repo, 
      ;;     git_reference_foreach_cb callback, void *payload);
      ((udecl
	(decl-spec-list (stor-spec (typedef)) . ,rst)
	(init-declr
	 (ftn-declr
	  (scope (ptr-declr (pointer) (ident ,typename)))
	  (param-list . ,params))))
       (let* ((ret-decl `(udecl (decl-spec-list . ,rst)
				(init-declr (ident "_"))))
	      (decl-return (gen-bs-decl-return ret-decl))
	      (decl-params (gen-bs-decl-params params)))
	 (fhscm-def-function* typename decl-return decl-params))
       (values (cons typename wrapped) (cons typename defined)))

      ;; typedef void* (*foo_t)(int x, ...);
      ((udecl
	(decl-spec-list (stor-spec (typedef)) . ,rst)
	(init-declr
	 (ptr-declr
	  (pointer)
	  (ftn-declr
	   (scope (ptr-declr (pointer) (ident ,typename)))
	   (param-list . ,params)))))
       (let* ((ret-decl `(udecl (decl-spec-list . ,rst)
				(init-declr (ptr-declr (pointer) (ident "_")))))
	      ;;(decl-return (gen-decl-return ret-decl))
	      ;;(decl-params (gen-decl-params params))
	      (decl-return (gen-decl-return ret-decl))
	      (decl-params (gen-decl-params params)))
	 (fhscm-def-function* typename decl-return decl-params))
       (values (cons typename wrapped) (cons typename defined)))

      ;; TODO: typedef void (foo_t)(int x)  [instead of *foo_t]
      ;; TODO: typedef void* (foo_t)(int x)

      ;; typedef foo_t bar_t
      ;; We retry with expansion of foo_t here.  Using fh-define-type-alias
      ;; was not working when we had "typedef struct foo foo_t;" But then
      ;; crashing on function types, so imported original type aliasing.
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (typename ,name)))
	(init-declr (ident ,typename)))
       (cond
	((member name bs-defined)
	 (values wrapped defined))
	((member name defined)
	 (sfscm "(define-public ~A-desc ~A-desc)\n" typename name)
	 (sfscm "(define-fh-type-alias ~A ~A)\n" typename name)
	 (sfscm "(export ~A)\n" typename)
	 (sfscm "(define-public ~A? ~A?)\n" typename name)
	 (sfscm "(define-public make-~A make-~A)\n" typename name)
	 (when (member (w/* name) defined)
	   (sfscm "(define-public ~A*-desc ~A*-desc)\n" typename name)
	   (sfscm "(define-fh-type-alias ~A* ~A*)\n" typename name)
	   (sfscm "(export ~A*)\n" typename)
	   (sfscm "(define-public ~A*? ~A*?)\n" typename name)
	   (sfscm "(define-public make-~A* make-~A*)\n" typename name))
	 (values (cons typename wrapped) (cons typename defined)))
	(else
	 (let ((xdecl (expand-typerefs udecl (*udict*) defined)))
	   (cnvt-udecl xdecl udict wrapped defined)))))

      ;; === structs and unions ==========

      ;; struct foo { ... }.
      ((udecl
	(decl-spec-list
	 (type-spec (struct-def (@ . ,attr1) (ident ,struct-name) ,field-list))))
       (cond
	((back-ref-getall udecl) =>
	 (lambda (name-list)
	   (cnvt-struct-def attr1 #f struct-name field-list)
	   (for-each
	    (lambda (typename)
	      (sfscm "(set! ~A-desc struct-~A-desc)\n" typename struct-name)
	      (fhscm-def-compound typename)
	      (fhscm-def-pointer (sw/* typename))
	      (fhscm-ref-deref typename))
	    name-list)
	   (values (cons (w/struct struct-name) wrapped)
		   (cons (w/struct struct-name) defined))))
	((not (member (w/struct struct-name) defined))
	 (cnvt-struct-def attr1 #f struct-name field-list)
	 ;; Hoping don't need w/struct*
	 (values (cons (w/struct struct-name) wrapped)
		 (cons (w/struct struct-name) defined)))
	(else
	 (values wrapped defined))))

      ;; struct { ... } ...
      ((udecl
	(decl-spec-list
	 (type-spec (struct-def ,field-list))))
       (sferr "bug in munge? unnamed struct-def\n")
       (pperr udecl)
       (values wrapped defined))

      ;; union foo { ... }.
      ((udecl
	(decl-spec-list
	 (type-spec (union-def (@ . ,attr1) (ident ,union-name) ,field-list))))
       (cond
	((back-ref-getall udecl) =>
	 (lambda (name-list)
	   ;;(sferr "  back-ref\n")
	   (cnvt-union-def #f #f union-name field-list)
	   (for-each
	    (lambda (typename)
	      (sfscm "(set! ~A-desc union-~A-desc)\n" typename union-name)
	      (fhscm-def-compound typename)
	      (fhscm-def-pointer (sw/* typename))
	      (fhscm-ref-deref typename))
	    name-list)
	   (values (cons (w/union union-name) wrapped)
		   (cons (w/union union-name) defined))))
	((not (member (w/union union-name) defined))
	 (cnvt-union-def attr1 #f union-name field-list)
	 (values (cons (w/union union-name) wrapped)
		 (cons (w/union union-name) defined)))
	(else
	 (values wrapped defined))))

      ;; union { ... } ...
      ((udecl
	(decl-spec-list
	 (type-spec (union-def ,field-list))))
       (sferr "bug in munge? unnamed union-def\n")
       (pperr udecl)
       (values wrapped defined))

      ;; === enums =======================

      ;; enum foo { ... };
      ((udecl
	(decl-spec-list
	 (type-spec (enum-def (ident ,enum-name) ,enum-def-list . ,rest))))
       (cnvt-enum-def #f enum-name enum-def-list)
       ;; probably never use this as arg to function
       (values (cons (w/enum enum-name) wrapped) defined))
      
      ;; enum { ... };
      ((udecl
	(decl-spec-list
	 (type-spec (enum-def ,enum-def-list . ,rest))))
       ;; This is now filtered in the caller so the C-decl is not printed.
       (values wrapped defined))

      ;; === function declarations =======
      
      ;; function returning pointer value
      ((udecl ,specl
	      (init-declr
	       (ptr-declr
		(pointer . ,rest)
		(ftn-declr (ident ,name) (param-list . ,params)))))
       (cnvt-fctn name (ptr-decl specl) params)
       (values wrapped defined))

      ;; function returning non-pointer value
      ;; TODO: parse ident part and process separately
      ((udecl ,specl
	      (init-declr
	       (ftn-declr (ident ,name) (param-list . ,params))))
       (cnvt-fctn name (non-ptr-decl specl) params)
       (values wrapped defined))
      ((udecl ,specl
	      (init-declr
	       (ftn-declr (scope (ident ,name)) (param-list . ,params))))
       (cnvt-fctn name (non-ptr-decl specl) params)
       (values wrapped defined))
      ((udecl ,specl
	      (init-declr
	       (ftn-declr (scope (ptr-declr (pointer . ,rest)
					    (ident ,name)))
			  (param-list . ,params))))
       (cnvt-fctn name (ptr-decl specl) params)
       (values wrapped defined))

      ;; === external variables =========

      ;; pointer
      ((udecl (decl-spec-list (stor-spec (extern)) ,type-spec)
	      (init-declr (ptr-declr (pointer) (ident ,name))))
       ;; This needs to have a delay and handler
       (let* ((udecl (expand-typerefs udecl (*udict*) (*defined*)))
	      (udecl (udecl-rem-type-qual udecl))
	      (mdecl (udecl->mdecl udecl)))
	 (cnvt-extern (car mdecl) (cdr mdecl)))
       (values wrapped defined))

      ;; non-pointer
      ((udecl (decl-spec-list (stor-spec (extern)) ,type-spec)
	      ,init-declr . ,rest)
       (let* ((udecl (expand-typerefs udecl (*udict*) (*defined*)))
	      (udecl (udecl-rem-type-qual udecl))
	      (mdecl (udecl->mdecl udecl)))
	 (cnvt-extern (car mdecl) (cdr mdecl))
	 (values wrapped defined)))

      ;; === special cases I need to fix =

      ;; from glib-2.0/gio.h
      ((udecl
	(decl-spec-list (stor-spec (typedef)) (type-spec (typename ,name)))
	(init-declr
	 (ptr-declr (pointer (pointer))
		    (scope (ftn-declr (scope (ptr-declr) (ident ,typename))
				      ,param-list)))))
       (sfscm "(define-public ~A-desc (bs:pointer 'void))\n" typename)
       (fhscm-def-pointer typename)
       (values (cons typename wrapped) (cons typename defined)))
      ((udecl
	(decl-spec-list (stor-spec (typedef)) (type-spec (typename ,name)))
	(init-declr
	 (ptr-declr (pointer (pointer))
		    (ftn-declr (scope (ptr-declr (pointer) (ident ,typename)))
			       ,param-list))))
       (sfscm "(define-public ~A-desc (bs:pointer 'void))\n" typename)
       (fhscm-def-pointer typename)
       (values (cons typename wrapped) (cons typename defined)))

      ;; from zzip/zzip.h
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (fixed-type ,typename))) ;; char
	(init-declr
	 (ptr-declr
	  (pointer (type-qual-list . ,rest))
	  (ident ,name))))
       (sfscm "(define-public ~A-desc (bs:pointer 'void))\n" typename)
       (fhscm-def-pointer typename)
       (values (cons typename wrapped) (cons typename defined)))

      ;; from hdf5.h
      ((udecl
	(decl-spec-list
	 (stor-spec (typedef))
	 (type-spec (fixed-type "unsigned char")))
	(init-declr
	 (array-of
	  (ident "hdset_reg_ref_t")
	  (add (sizeof-type
		(type-name
		 (decl-spec-list (type-spec (typename "haddr_t")))))
	       (p-expr (fixed "4"))))))
       (let* ((typename "haddr_t")
	      (size (+ (sizeof '*) 4)))
	 (sfscm "(define-public ~A-desc (bs:vector ~A '*))\n" typename size)
	 ;;(sfscm "(define-fh-compound-type/p ~A ~A-desc)\n" typename typename)
	 (fhscm-def-compound typename)
	 (values (cons typename wrapped) (cons typename defined))))

      ;; from gtk+-3.0/gtk/gtk.h
      ((udecl (decl-spec-list
	       (stor-spec (typedef))
	       (type-spec (fixed-type "char")))
	      (init-declr
	       (ptr-declr (pointer) (ident "GtkStock"))))
       (sferr "missed gtk3 decl not expanded\n")
       (values wrapped defined))

      ;; from uuid.h
      ((udecl (decl-spec-list
	       (stor-spec (typedef))
	       (type-spec (fixed-type "unsigned char")))
	      (init-declr
	       (array-of (ident ,typename) (p-expr (fixed ,len)))))
       (sfscm "(define-public ~A-desc (bs:vector ~A uint8))\n" typename len)
       (fhscm-def-compound typename)
       (values (cons typename wrapped) (cons typename defined)))

      ;; === missed =====================

      (,otherwise
       (sferr "see below:\n")
       (pperr udecl)
       ;;(sferr "-\n")
       ;;(pperr `(udecl ,specl ,declr))
       (error "cnvt-udecl")
       (fherr "cnvt-udecl missed --^")
       (values wrapped defined)))))


;; === enums and #defined => lookup

;; given keeper-defs (k-defs) and cpp defs (c-defs) expand the keeper
;; replacemnts down to constants (strings, integers, etc)
(define (gen-lookup-proc keep-defs cpp-defs ext-mods)

  (define err-port (open-output-file "/dev/null"))
  
  (define* (try-parse-repl repl)
    (with-error-to-port err-port
      (lambda ()
	(catch 'c99-error
	  (lambda () (parse-c99x repl (*tdefs*) #:cpp-defs cpp-defs))
	  (lambda args #f)))))

  ;; @var{keep-defs} is list of CPP defs and enum key/val pairs. It is
  ;; possible for an enum symbol to be used as a macro function so we
  ;; need to first check for integer before trying expand-cpp-macro-ref.
  (sfscm "\n;; access to enum symbols and #define'd constants:\n")
  (let ((st-name (string->symbol (string-append (*prefix*) "-symbol-tab")))
	(sv-name (string->symbol (string-append (*prefix*) "-symbol-val")))
	(defs
	  (fold
	   (lambda (def seed)
	     (let* ((name (car def)) (val (cdr def))
		    (symb (string->symbol name))
		    (repl (cond
			   ((pair? val) #f)
			   ((string->number (cdr def)) (cdr def))
			   (else
			    ;; or maybe should export/use cpp-expand-text
			    (with-input-from-string ""
			      (lambda ()
				(expand-cpp-macro-ref name cpp-defs)))))))
	       ;; TODO: try to reduce all this to the parse-c99x one
	       (cond
		((not repl) seed)
		((not (string? repl)) (sferr "not string: ~S\n" repl))
		((zero? (string-length repl)) seed)
		;;
		((cintstr->num repl) => (lambda (val) (acons symb val seed)))
		((try-parse-repl repl)
		 => (lambda (val)
		      (let ((cv (eval-c99-cx val (*udict*) (*ddict*))))
			(unless cv
			  (sfscm ";; unable to generate constant for ~S\n" name))
			(if cv (acons symb cv seed) seed))))
		;;
		(else
		 ;;(sferr "gen-lookup-proc misssed ~A ~S\n" name repl)
		 seed))))
	   '()
	   keep-defs))
	(ext-ftns			; lookup in use-ffi-modules
	 (map
	  (lambda (mod)
	    (list (string->symbol
		   (string-append (path->name mod) "-symbol-val")) 'k))
	  ext-mods)))
    (ppscm `(define ,st-name '(,@defs)))
    (ppscm `(define ,sv-name (lambda (k) (or (assq-ref ,st-name k) ,@ext-ftns))))
    (sfscm "(export ~A)\n" sv-name)
    ;;
    (nlscm)
    (ppscm
     `(define (unwrap-enum obj)
	(cond
	 ((number? obj) obj)
	 ((symbol? obj) (,sv-name obj))
	 ((fh-object? obj) (struct-ref obj 0)) ;; ???
	 (else (error "type mismatch")))))
    
    (close err-port)))


;; === Parsing the C header(s)

;; @deffn parse-code code [attrs]
;; Parse @var{code}, a Scheme string, using cpp-defs and inc-dirs from
;; @var{attrs}.
;; This procedure is used by @code{parse-includes}.
;; @end deffn
(define* (parse-code code #:optional (attrs '()))
  (let* ((cpp-defs (resolve-attr-val (assq-ref attrs 'cpp-defs)))
	 (inc-dirs (resolve-attr-val (assq-ref attrs 'inc-dirs)))
	 (inc-help (resolve-attr-val (assq-ref attrs 'inc-help)))
	 ;;
	 (pkg-config (assq-ref attrs 'pkg-config))
	 (cpp-defs (append (pkg-config-defs pkg-config) cpp-defs))
	 (inc-dirs (append (pkg-config-incs pkg-config) inc-dirs))
	 ;;
	 (cpp-defs (append cpp-defs fh-cpp-defs))
	 (inc-dirs (append inc-dirs fh-inc-dirs))
	 (inc-help (append inc-help fh-inc-help)))
    (or (with-input-from-string code
	  (lambda ()
	    (parse-c99 #:cpp-defs cpp-defs
		       #:inc-dirs inc-dirs
		       #:inc-help inc-help
		       #:mode 'decl
		       #:show-incs #f
		       #:debug (*debug-parse*))))
	(fherr "parse failed"))))

;; @deffn parse-includes attrs
;; This routine generates a top-level source string-file with all the includes,
;; parses it, and then merges one level down of includes into the top level,
;; as if the bodies of the incudes had been combined into one file.
;; @end deffn
(define parse-includes
  (let* ((p (node-join
	     (select-kids (node-typeof? 'cpp-stmt))
	     (select-kids (node-typeof? 'include))
	     (select-kids (node-typeof? 'trans-unit))))
	 (merge-inc-bodies
	  (lambda (t) (cons 'trans-unit (apply append (map cdr (p t)))))))
    (lambda (attrs)
      (let* ((inc-files (resolve-attr-val (assq-ref attrs 'include)))
	     (prog (string-join
		    (map
		     (lambda (inc-file)
		       (string-append "#include \"" inc-file "\"\n"))
		     inc-files) "")))
	(and=> (parse-code prog attrs) merge-inc-bodies)))))

;; === main converter ==================

(define (derive-dirpath sfile mbase)
  (if (not sfile) "./"
      (let* ((sbase (string-drop-right sfile 4))
	     (sfxln (string-suffix-length sbase mbase))
	     (sblen (string-length sbase)))
	(if (not (= sfxln (string-length mbase))) ; need more robust
	    (error "filename-path inconsistent"))
	(substring sbase 0 (- sblen sfxln)))))

;; Return #t when ffi-file has an mtime greater than that of scm-file
(define (more-recent? ffi-file scm-file)
  ;; copied from ice-9/boot-9.scm
  (let ((stat1 (stat ffi-file)) (stat2 (stat scm-file)))
    (or (> (stat:mtime stat1) (stat:mtime stat2))
	(and (= (stat:mtime stat1) (stat:mtime stat2))
	     (>= (stat:mtimensec stat1)
		 (stat:mtimensec stat2))))))

;; given modules spec, determine if any ffi-module dependencies are
;; outdated
(define (check-deps module-options)
  (let ((ext-modz ;; use filter?
	 (fold-right
	  (lambda (opt seed)
	    (if (eq? (car opt) 'use-ffi-module) (cons (cdr opt) seed) seed))
	  '() module-options))
	(ext-mods (map cdr (filter
			    (lambda (opt) (eq? (car opt) 'use-ffi-module))
			    module-options))))
    (for-each
     (lambda (fmod)
       (let* ((base (string-join (map symbol->string fmod) "/"))
	      (xffi (string-append base ".ffi"))
	      (xscm (string-append base ".scm")))
	 (when (not (access? xscm R_OK))
	   (fherr "compiled dependent ~S not found" fmod))
	 (when (more-recent? xffi xscm)
	   (fherr "dependent ~S needs recompile" xffi)
	   (sleep 2))))
     ext-mods)))

;; => (values wrapped defined)
(define* (process-decls decls udict
			#:optional (wrapped '()) (defined '())
			#:key (declf (lambda (k) #t))
			)
  (let* () ;;(declf (if declf declf (lambda (key) #t))))
    (fold-values			  ; from (sxml fold)
     (lambda (name wrapped defined) ; name: "foo_t" or (enum . "foo")
       (catch 'ffi-help-error
	 (lambda ()
	   (cond
	    ((and ;; Process the declaration if all conditions met:
	      (declf name)		  ; 1) user wants it
	      (not (member name defined)) ; 2) not already defined
	      (not (and (pair? name)	  ; 3) not anonymous
			(string=? "*anon*" (cdr name)))))
	     (let ((udecl (udict-ref udict name)))
	       (nlscm) (c99scm udecl)
	       (if (*echo-decls*)
		   (sfscm "(if echo-decls (display \"~A\\n\"))\n" name))
	       (cnvt-udecl udecl udict wrapped defined)))
	    (else (values wrapped defined))))
	 ;; exception handler:
	 (lambda (key fmt . args)
	   (if fmt (apply simple-format (current-error-port)
			  (string-append "ffi-help: " fmt "\n") args))
	   (sfscm ";; ... failed.\n")
	   (values wrapped defined))))
     decls wrapped defined)))

;; process define-ffi-module expression
;; was intro-ffi
(define (expand-ffi-module-spec path module-options)
  (check-deps module-options)
  (let* ((script-options (*options*))
	 (dbugl (or (and=> (assq-ref script-options 'debug)
			   (lambda (v) (map string->symbol
					    (string-split v #\,))))
		    '()))
	 (mbase (path->path path))
	 (dirpath (derive-dirpath (assq-ref script-options 'file) mbase))
	 (mfile (string-append dirpath mbase ".scm"))
	 (mport (open-output-file mfile))
	 ;;
	 (attrs (opts->attrs module-options script-options))
	 (incf (or (assq-ref attrs 'inc-filter) #f))
	 (declf (or (assq-ref attrs 'decl-filter) identity))
	 (renamer (or (assq-ref attrs 'renamer) identity))
	 ;;
	 (tree (begin
		 (if (memq 'parse dbugl) (*debug-parse* #t))
		 (cond
		  ((assq-ref attrs 'include) (parse-includes attrs))
		  ((assq-ref attrs 'api-code) =>
		   (lambda (code) (parse-code code attrs)))
		  (else (fherr "expecing #:include or #:api-code")))))
	 (udecls (c99-trans-unit->udict tree #:inc-filter incf))
	 (udict (c99-trans-unit->udict/deep tree))
	 (ffi-decls (map car udecls))	; just the names, get decls from udict

	 ;; OK, I think this is fixed now.  Was ...
	 ;; 1. If udict, then exported symbols looks good, but ref's don't work
	 ;; 2. If udecls, refs work but bloated symval struct.
	 ;; The conflict is in
	 ;; const-expr->number VS call to gen-lookup-proc 1st arg ffi-defs
	 (ffi-enu-defs (udict-enums->ddict udecls))
	 (ffi-defs (c99-trans-unit->ddict tree ffi-enu-defs #:inc-filter incf))
	 (cpp-defs (c99-trans-unit->ddict tree #:inc-filter #t))
	 (all-enu-defs (udict-enums->ddict udict))
	 (all-defs (c99-trans-unit->ddict
		    tree all-enu-defs #:inc-filter #t #:skip-fdefs #t))
	 (ddict all-defs)
	 ;; the list of typedefs we will generate (later):
	 (ffimod-defined #f)
	 ;; ext modules [from #:use-modules (ffi cairo)]
	 (ext-mods
	  (fold-right
	   (lambda (opt seed)
	     (if (eq? (car opt) 'use-ffi-module) (cons (cdr opt) seed) seed))
	   '() module-options))
	 ;; list of exernal defined
	 (ext-defd			
	  (fold
	   (lambda (upath seed)
	     (unless (resolve-module upath)
	       (error "module not defined:" upath))
	     (let* ((modul (resolve-module upath #:ensure #f))
		    (pname (path->name upath))
		    (vname (string->symbol (string-append pname "-types")))
		    (var (module-ref modul vname)))
	       (append var seed)))
	   '() ext-mods)))
    ;; set globals
    (*prefix* (path->name path))
    (*udict* udict)
    (*mport* mport)
    (*ddict* ddict)
    (*renamer* renamer)
    (*tdefs* (udict->typenames udict))
    (if (memq 'echo-decls dbugl) (*echo-decls* #t))

    ;; file and module header
    (ffimod-header path module-options)

    ;; Convert and output foreign declarations.
    (call-with-values
	(lambda ()
	  ;; We need to have externs in wrapped because function param
	  ;; type have wrapped types preserved (e.g., enums).
	  ;; swap of udecls with udict failed on glib g???
	  (process-decls ffi-decls udecls ;; udict <= swap failed 01 Dec 2018
			 ;; wrapped and defined:
			 ext-defd (append bs-defined ext-defd)
			 ;; declaration filter
			 #:declf declf))
      (lambda (wrapped defined)
	;; Set ffimod-defined for including, but removed built-in types.
	(let* ((bity (car bs-defined))	; first built-in type
	       (defd (let loop ((res '()) (defs defined))
		       (if (eq? (car defs) bity) res
			   (loop (cons (car defs) res) (cdr defs))))))
	  (set! ffimod-defined defd))))
    
    ;; output global constants (from enum and #define)
    (gen-lookup-proc ffi-defs cpp-defs ext-mods)

    ;; output list of defined types
    (sfscm "\n(define ~A-types\n  '" (path->name path))
    (ugly-print ffimod-defined mport #:per-line-prefix "   " #:trim-ends #t)
    (sfscm ")\n(export ~A-types)\n" (path->name path))

    ;; Return the output port so the compiler can output remaining code.
    mport))


;; === test compiler ================

;; @deffn {Procedure} C-fun-decl->scm code
;; Generate a symbolic expression that evals to a Guile procedure.
;; @example
;; (define fmod-exp (C-fun-decl->proc "double dmod(double, double);"))
;; (define fmod (eval fmod-exp (current-module)))
;; (fmod 2.3 0.5)
;; @end example
;; @end deffn
(define (C-fun-decl->scm code)
  (let ((tree (with-input-from-string code parse-c99)))
    (if tree
	(let* ((udict (unitize-decl (sx-ref tree 1)))
	       (name (caar udict)) (udecl (cdar udict))
	       (gen1 (fh-cnvt-udecl udecl '()))
	       (gen2 (with-input-from-string gen1 read))
	       (gen3 (caddr gen2)))
	  gen3))))

(define* (fh-cnvt-udecl udecl udict #:key (prefix "fh"))
  (parameterize ((*options* '()) (*wrapped* '()) (*defined* '())
		 (*renamer* identity) (*errmsgs* '()) (*prefix* prefix)
		 (*mport* (open-output-string)) (*udict* udict))
    (cnvt-udecl udecl udict '() '())
    (let ((res (get-output-string (*mport*))))
      (close (*mport*))
      res)))

;; convert string-body of Scheme code to a Scheme expression
;; @example
;; (fh-scm-str->scm-exp "(define a 1)") => '(begin (define a 1))
;; @end example
(define (fh-scm-str->scm-exp str)
  (call-with-input-string str
    (lambda (iport)
      (cons 'begin
	    (let loop ((exp (read iport)))
	      (if (eof-object? exp) '()
		  (cons exp (loop (read iport)))))))))

;; Convert declaration with @var{name} in string-body of C @var{code}
;; to string-body of Scheme code.
;; @example
;; (fh-cnvt-cdecl "sqrt" "double sqrt(double x);") =>
;;   "(define ~sqrt ...)\n (define (sqrt x) ...)"
;; @end example
(define* (fh-cnvt-cdecl->str name code #:key (prefix "fh"))
  (let* ((tree (with-input-from-string code parse-c99))
	 (udict (c99-trans-unit->udict tree))
	 (udecl (assoc-ref udict name)))
    (fh-cnvt-udecl udecl udict)))

;; like above but then convert to Scheme expression
(define* (fh-cnvt-cdecl name code #:key (prefix "fh"))
  (and=> (fh-cnvt-cdecl->str name code #:prefix prefix) fh-scm-str->scm-exp))
			     
;; === repl compiler ================

;; @deffn {Procedure} load-include-file filename [pkg-config]
;; This is the functionality that Ludo was asking for: to be at guile
;; prompt and be able to issue
;; @example
;; (use-modules (nyacc lang c99 ffi-help))
;; (load-include-file "cairo.h" #:pkg-config "cairo")
;; @end example
;; @end deffn
;; + Right now the only way would be to generate a file and eval it, because
;;   our code generates strings and not lists.
;; + and=> with-output-to-string  eval
;; + first need to cut up intro-ffi

;; options:
;;   api-code cpp-defs decl-filter
;;   inc-dirs inc-filter inc-help include
;;   library pkg-config renamer
(define* (load-include-file filename
			    #:key pkg-config)
  (parameterize ((*options* '()) (*wrapped* '()) (*defined* '())
		 (*renamer* identity) (*errmsgs* '())
		 (*prefix* "") (*mport* #t) (*udict* '()))
    (let* ((attrs (acons 'include (list filename) '()))
	   (attrs (if pkg-config (acons 'pkg-config pkg-config attrs) attrs))
	   (tree (parse-includes attrs))
	   (udict (c99-trans-unit->udict/deep tree))
	   (udecls (c99-trans-unit->udict tree))
	   (decls (map car udecls))
	   (scmport (mkstemp! (string-copy ",_FH_XXXXXX")))
	   ;;(scmport (tmpfile)) ;; does not work ?
	   (scmfile (port-filename scmport))
	   (compile-file (@@ (system base compile) compile-file)))
      (*prefix* (symbol->string (gensym "fh-")))
      (*mport* scmport)
      (*udict* udict)
      (sfscm "(use-modules (system ffi-help-rt))\n")
      (sfscm "(use-modules ((system foreign) #:prefix ffi:))\n")
      (sfscm "(use-modules (bytestructures guile))\n")
      (ppscm `(define ,(link-libs)
		(list ,@(map
			 (lambda (l) `(dynamic-link ,l))
			 (pkg-config-libs pkg-config)))))
      (process-decls decls udict '() bs-defined)
      (close (*mport*))
      (simple-format #t "wrote ~S; compile and load: ...\n" scmfile)
      (load-compiled (compile-file scmfile #:opts '()))
      (if #f #f))))


;; === file compiler ================

(use-modules (system base language))
(use-modules (ice-9 pretty-print))

;; This macro converts #:key val to '(key val) for ffi-help options
;; and preserves other #:key-val pairs for passthrough to the module
;; Note that keywords are converted to symbols before they get here
;; NOT USED.
(define-syntax parse-ffimod-option
  (lambda (x)
    (define (sym->key stx)
      (datum->syntax stx (symbol->keyword (syntax->datum stx))))
    (syntax-case x (api-code
		    cpp-defs decl-filter inc-dirs inc-filter inc-help include
		    library pkg-config renamer)
      ((_ api-code code) #'(cons 'api-code code))
      ((_ cpp-defs proc) #'(cons 'cpp-defs proc))
      ((_ decl-filter proc) #'(cons 'decl-filter proc))
      ((_ inc-dirs proc) #'(cons 'inc-dirs proc))
      ((_ inc-filter proc) #'(cons 'inc-filter proc))
      ((_ inc-help expr) #'(cons 'inc-help expr))
      ((_ include expr) #'(cons 'include expr))
      ((_ library expr) #'(cons 'library expr)) ;; eval to list of libs
      ((_ pkg-config string) #'(cons 'pkg-config string))
      ((_ renamer proc) #'(cons'renamer (quote proc)))
      ;;((_ use-ffi-module path) #'(cons 'use-ffi-module (quote path)))
      ;; remaining options get passed to the module decl as-is:
      ;;((_ key val) #`(cons #,(sym->key #'key) (quote val)))
      ;;((_ key val) #`(cons #,(symbol->keyword #'key) (quote val)))
      )))

(define-syntax parse-module-options
  (lambda (x)
    (define (key->sym stx)
      (datum->syntax stx (keyword->symbol (syntax->datum stx))))
    (define (ffimod-option? key)
      (and (keyword? key)
	   (member key '(#:api-code
			 #:cpp-defs #:decl-filter #:inc-dirs #:inc-filter
			 #:inc-help #:include #:library #:pkg-config #:renamer
			 #:use-ffi-module))))
    (define (module-option? key) (keyword? key))

    (syntax-case x ()
      ((_ key val option ...)
       (eq? (syntax->datum #'key) #:use-ffi-module)
       #`(cons
	  (cons (quote #,(key->sym #'key)) (quote val))
	  (parse-module-options option ...)))

      ((_ key val option ...)
       (ffimod-option? (syntax->datum #'key))
       #`(cons
	  (cons (quote #,(key->sym #'key)) val)
	  (parse-module-options option ...)))

      ((_ key val option ...)
       (module-option? (syntax->datum #'key))
       #`(cons
	  (cons key (quote val))
	  (parse-module-options option ...)))
      
      ((_ key val option ...)
       #'(syntax-error "compile-ffi: expecting keyword"))
      
      ((_) #''()))))

(define-syntax-rule (define-ffi-module path-list attr ...)
  (expand-ffi-module-spec (quote path-list) (parse-module-options attr ...)))

(define (string-member-proc . args)
  (lambda (s) (member s args)))

;; to convert symbol-based #:renamer to string-based
(define (string-renamer proc)
  (lambda (s) (string->symbol (proc (symbol->string s)))))

(define scm-reader (language-reader (lookup-language 'scheme)))

;; @deffn {Procedure} compile-ffi-file file [options]
;; This procedure will 
;; @end deffn
(define* (compile-ffi-file file #:optional (options '()))
  (parameterize ((*options* options) (*wrapped* '()) (*defined* '())
		 (*renamer* identity) (*errmsgs* '()) (*prefix* "")
		 (*mport* #t) (*udict* '()) (*ddict* '()))
    ;; if not interactive ...
    (debug-disable 'backtrace)
    (if (not (access? file R_OK))
	(throw 'ffi-help-error "ERROR: not found: ~S" file))
    (call-with-input-file file
      (lambda (iport)
	(let ((env (make-fresh-user-module)))
	  (eval '(use-modules (nyacc lang c99 ffi-help)) env)
	  (let loop ((oport #f) (exp (read iport)))
	    (cond
	     ((eof-object? exp)
	      (when oport
		(display "\n;; --- last line ---\n" oport)
		(close-port oport)))
	     ((and (pair? exp) (eqv? 'define-ffi-module (car exp)))
	      (loop (eval exp env) (read iport)))
	     (else
	      (when oport
		(newline oport)
		(pretty-print exp oport))
	      (loop oport (read iport))))))))))

;; --- last line ---

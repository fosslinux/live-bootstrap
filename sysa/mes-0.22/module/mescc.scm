;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright © 2016,2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(define-module (mescc)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 getopt-long)
  #:use-module (mes guile)
  #:use-module (mes misc)
  #:use-module (mescc mescc)
  #:export (mescc:main))

(cond-expand
 (mes
  (define (set-port-encoding! port encoding) #t)
  (mes-use-module (mes guile))
  (mes-use-module (mes misc))
  (mes-use-module (mes getopt-long))
  (mes-use-module (mes display))
  (mes-use-module (mescc mescc))
  )
 (guile
  (define-macro (mes-use-module . rest) #t)))

(define %host-arch (or (getenv "%arch") %arch))
(define %host-kernel (or (getenv "%kernel") "linux")) ;; FIXME
(define %prefix (or (getenv "%prefix") "mes"))
(define %includedir (or (getenv "%includedir") "include"))
(define %libdir (or (getenv "%libdir") "."))
(define %version (or (getenv "%version") "0.0"))
(define %numbered-arch? (and=> (getenv "%numbered_arch") (lambda (x) (equal? x "true"))))

(when (and=> (getenv "V") (lambda (v) (and (= (string-length v) 1) (> (string->number v) 1))))
  (format (current-error-port) "mescc[~a]...\n" %scheme))

(define (unclump-single o)
  (cond ((string-prefix? "--" o) (list o))
        ((and (string-prefix? "-" o)
              (> (string-length o) 2)
              (not (eq? (string-ref o 2) #\space)))
         (list (substring o 0 2)
               (substring o 2)))
        (else (list o))))

(define (parse-opts args)
  (let* ((option-spec
          '((align)
            (arch (value #t))
            (assemble (single-char #\c))
            (base-address (value #t))
            (compile (single-char #\S))
            (define (single-char #\D) (value #t))
            (debug-info (single-char #\g))
            (dumpmachine)
            (fno-builtin)
            (fno-stack-protector)
            (help (single-char #\h))
            (include (single-char #\I) (value #t))
            (library-dir (single-char #\L) (value #t))
            (library (single-char #\l) (value #t))
            (machine (single-char #\m) (value #t))
            (nodefaultlibs)
            (nostartfiles)
            (nostdinc)
            (nostdlib)
            (numbered-arch?)
            (preprocess (single-char #\E))
            (static)
            (std (value #t))
            (output (single-char #\o) (value #t))
            (optimize (single-char #\O) (value #t))
            (version (single-char #\V))
            (verbose (single-char #\v))
            (write (single-char #\w) (value #t))
            (language (single-char #\x) (value #t))))
         (options (getopt-long args option-spec))
         (help? (option-ref options 'help #f))
         (files (option-ref options '() '()))
         (dumpmachine? (option-ref options 'dumpmachine #f))
         (version? (option-ref options 'version #f))
         (usage? (and (not dumpmachine?) (not help?) (not version?) (null? files))))
    (cond (version? (format #t "mescc (GNU Mes) ~a\n" %version) (exit 0))
          (else
           (and (or help? usage?)
                (format (or (and usage? (current-error-port)) (current-output-port)) "\
Usage: mescc [OPTION]... FILE...
C99 compiler in Scheme for bootstrapping the GNU system.

Options:
  --align             align globals
  --arch=ARCH         compile for ARCH [~a]
  --kernel=ARCH       compile for KERNEL [~a]
  -dumpmachine        display the compiler's target machine
  --base-address=ADRRESS
                      use BaseAddress ADDRESS [0x1000000]
  --numbered-arch     mescc-tools use numbered arch
  -D DEFINE[=VALUE]   define DEFINE [VALUE=1]
  -E                  preprocess only; do not compile, assemble or link
  -g                  add debug info [GDB, objdump] TODO: hex2 footer
  -h, --help          display this help and exit
  -I DIR              append DIR to include path
  -L DIR              append DIR to library path
  -l LIBNAME          link with LIBNAME
  -m BITS             compile for BITS bits [32]
  -nodefaultlibs      do not use libc.o when linking
  -nostartfiles       do not use crt1.o when linking
  -nostdlib           do not use crt1.o or libc.o when linking
  -o FILE             write output to FILE
  -O LEVEL            use optimizing LEVEL
  -S                  preprocess and compile only; do not assemble or link
  --std=STANDARD      assume that the input sources are for STANDARD
  -V,--version        display version and exit
  -w,--write=TYPE     dump Nyacc AST using TYPE {pretty-print,write}
  -x LANGUAGE         specify LANGUAGE of the following input files

Ignored for GCC compatibility
  -fno-builtin
  -fno-stack-protector
  -no-pie
  -nostdinc
  -static

Environment variables:

  MES=BINARY          run on mes-executable BINARY {mes,guile}
  MES_DEBUG=LEVEL     show debug output with verbosity LEVEL {0..5}
  NYACC_TRACE=1       show Nyacc progress

Report bugs to: bug-mes@gnu.org
GNU Mes home page: <http://gnu.org/software/mes/>
General help using GNU software: <http://gnu.org/gethelp/>
" %host-arch %host-kernel)
                (exit (or (and usage? 2) 0)))
           options))))

(define (mescc:main args)
  (let* ((single-dash-options '("-dumpmachine"
                                "-fno-builtin"
                                "-fno-stack-protector"
                                "-no-pie"
                                "-nodefaultlibs"
                                "-nostartfiles"
                                "-nostdinc"
                                "-nostdlib"
                                "-static"
                                "-std"))
         (args (map (lambda (o)
                      (if (member o single-dash-options) (string-append "-" o)
                          o))
                    args))
         (args (append-map unclump-single args))
         (options (parse-opts args))
         (options (acons 'prefix %prefix options))
         (options (acons 'includedir %includedir options))
         (options (acons 'libdir %libdir options))
         (arch (option-ref options 'arch %host-arch))
         (options (if arch (acons 'arch arch options) options))
         (kernel (option-ref options 'kernel %host-kernel))
         (options (acons 'kernel kernel options))
         (numbered-arch? (option-ref options 'numbered-arch? %numbered-arch?))
         (options (acons 'numbered-arch? numbered-arch? options))
         (dumpmachine? (option-ref options 'dumpmachine #f))
         (preprocess? (option-ref options 'preprocess #f))
         (compile? (option-ref options 'compile #f))
         (assemble? (option-ref options 'assemble #f))
         (verbose? (count-opt options 'verbose)))
    (when verbose?
      (setenv "NYACC_TRACE" "yes")
      (when (> verbose? 1)
        (format (current-error-port) "options=~s\n" options)))
    (cond (dumpmachine? (display (mescc:get-host options)))
          (preprocess? (mescc:preprocess options))
          (compile? (mescc:compile options))
          (assemble? (mescc:assemble options))
          (else (mescc:link options)))))

(define main mescc:main)

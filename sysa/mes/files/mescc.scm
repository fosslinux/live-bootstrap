; SPDX-FileCopyrightText: Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
; SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
;
; SPDX-License-Identifier: GPL-3.0-or-later

(cond-expand
 (mes)
 (guile
  (define %arch (car (string-split %host-type #\-)))
  (define %kernel (car (filter
                        (compose not
                                 (lambda (x) (member x '("pc" "portbld" "unknown"))))
                        (cdr (string-split %host-type #\-)))))))

(define %prefix (or (getenv "MES_PREFIX")
                      (if (string-prefix? "@prefix" "/usr")
                          ""
                          "/usr")))

(define %includedir (or (getenv "includedir")
                        (string-append %prefix "/include")))

(define %libdir (or (getenv "libdir")
                    (string-append %prefix "/lib")))

(define %version (if (string-prefix? "@VERSION" "0.22-m2") "git"
                     "0.22"))

(define %arch (if (string-prefix? "@mes_cpu" "x86") %arch
                  "x86"))

(define %kernel (if (string-prefix? "@mes_kernel" "linux") %kernel
                    "linux"))

(define %numbered-arch? (if (getenv "numbered_arch") (and=> (getenv "numbered_arch")
                                                            (lambda (x) (equal? x "true")))
                            (if (string-prefix? "@numbered_arch" "false") #f
                                (equal? "false" "true"))))

(setenv "%prefix" %prefix)
(setenv "%includedir" %includedir)
(setenv "%libdir" %libdir)
(setenv "%version" %version)
(setenv "%arch" %arch)
(setenv "%kernel" %kernel)
(setenv "%numbered_arch" (if %numbered-arch? "true" "false"))

(cond-expand
 (mes
  (mes-use-module (mescc))
  (mescc:main (command-line)))
 (guile
  (use-modules (mescc))))

(define (main args)
  (mescc:main args))

;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright © 2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

;; The GNU project defaults.  These are also the GNU Emacs defaults.
;; Re-asserting theme here, however, as a courtesy for setups that use
;; a global override.
(
 ;; For writing GNU C code, see
 ;; https://www.gnu.org/prep/standards/html_node/Writing-C.html
 (c-mode . ((c-file-style . "gnu")
            (indent-tabs-mode . nil)))

 (makefile-mode . ((indent-tabs-mode . t)))
 (asm-mode . ((indent-tabs-mode . t)))

 (nil . ((indent-tabs-mode . nil)
         (fill-column . 72)))

 (scheme-mode
  .
  ((geiser-active-implementations . (guile))
   (eval
    .
    (progn
      (defun prefix-dir-locals-dir (elt)
        (concat (locate-dominating-file buffer-file-name ".dir-locals.el") elt))
      (mapcar
       (lambda (dir) (add-to-list 'geiser-guile-load-path dir))
       (mapcar
        #'prefix-dir-locals-dir
        '("scripts" "module")))))))

 (texinfo-mode    . ((indent-tabs-mode . nil)
                     (fill-column . 72)))
 (nil .
      ((eval
        .
        (progn
	  (let ((top (locate-dominating-file default-directory ".dir-locals.el"))))

	  (defun guile--manual-look-up (id mod)
            (message "guile--manual-look-up id=%s => %s mod=%s" id (symbol-name id) mod)
            (let ((info-lookup-other-window-flag
		   geiser-guile-manual-lookup-other-window-p))
              (info-lookup-symbol (symbol-name id) 'scheme-mode))
            (when geiser-guile-manual-lookup-other-window-p
              (switch-to-buffer-other-window "*info*"))
            (search-forward (format "%s" id) nil t))

	  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)

	  (defun guix-switch-profile (&optional profile)
            "reset Emacs' environment by snarfing PROFILE/etc/profile"

            (defun matches-in-string (regexp string)
              "return a list of matches of REGEXP in STRING."
              (let ((matches))
	        (save-match-data
		  (string-match "^" "")
		  (while (string-match regexp string (match-end 0))
                    (push (or (match-string 1 string) (match-string 0 string)) matches)))
	        matches))

            (interactive "fprofile: ")
            (let* ((output (shell-command-to-string (concat "GUIX_PROFILE= /bin/sh -x " profile "/etc/profile")))
		   (exports (matches-in-string "^[+] export \\(.*\\)" output)))
              (mapcar (lambda (line) (apply #'setenv (split-string line "="))) exports )))

	  (defun shell-args-to-string (&rest args)
            (shell-command-to-string (mapconcat 'identity args " ")))

	  (defun as (string &optional arch)
            (let* ((arch (or arch "--64"))
		   (asm (subst-char-in-string ?_ ?\s string))
		   (foo (message "asm:%S" asm))
		   (result (shell-args-to-string "as" arch (concat "<(echo '" asm "')")))
		   (disassembly (shell-args-to-string "objdump" "-d" "a.out"))
		   (foo (message "disassembly: %S" disassembly))
		   (match (string-match "^   0:[\t]\\([^\t]*\\)" disassembly))
		   (code (match-string 1 disassembly))
		   (code (apply 'concat (split-string code " " t))))
              (insert " ")
              (insert code)))

	  (defun as-32 (point mark)
            (interactive "r")
            (let* ((string (buffer-substring point mark))
		   (code (as string "--32")))
              (insert " ")
              (insert code)))

	  (defun as-64 (point mark)
            (interactive "r")
            (let* ((string (buffer-substring point mark))
		   (code (as string "--64")))
              (insert " ")
              (insert code))))))))

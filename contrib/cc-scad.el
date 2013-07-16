;;; cc-scad.el --- A major mode for editin OpenSCAD files based on cc-mode.

;; Copyright (C) 2013 Łukasz Stelmach

;; Author: Łukasz Stelmach <stlman@poczta.fm> (based on cc-awk.el and scad-mode.el)
;; Maintainer:
;; Keywords: SCAD, cc-mode, languages
;; Package: OpenSCAD

;; This programme is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Comentary:

;; This file contains the adapations to cc-mode required for editing
;; OpenSCAD files.

;;; Code:

(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (load "cc-bytecomp" nil t)))

(require 'cc-mode)

;; ;; Silence the byte compiler.
;; (cc-bytecomp-defvar font-lock-mode)	; Checked with boundp before use.
;; (cc-bytecomp-defvar c-new-BEG)
;; (cc-bytecomp-defvar c-new-END)

;; ;; Some functions in cc-engine that are used below.  There's a cyclic
;; ;; dependency so it can't be required here.  (Perhaps some functions
;; ;; could be moved to cc-engine to avoid it.)
;; (cc-bytecomp-defun c-backward-token-1)
;; (cc-bytecomp-defun c-beginning-of-statement-1)
;; (cc-bytecomp-defun c-backward-sws)

(defvar scad-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; support comment style (b): “// ...” 
    ;; support comment style (a): “/* ... */” 
    (modify-syntax-entry ?\/ ". 124b" st)
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?* ". 23" st)

    ;; Extra punctuation
    (modify-syntax-entry ?+  "." st)
    (modify-syntax-entry ?-  "." st)
    (modify-syntax-entry ?%  "." st)
    (modify-syntax-entry ?<  "." st)
    (modify-syntax-entry ?>  "." st)
    (modify-syntax-entry ?&  "." st)
    (modify-syntax-entry ?:  "." st)
    (modify-syntax-entry ?|  "." st)
    (modify-syntax-entry ?=  "." st)
    (modify-syntax-entry ?\;  "." st)

    ;; _ allowed in word (alternatively "_" as symbol constituent?)
    (modify-syntax-entry ?_  "_" st)

    st)
  "Syntax table for `scad-mode'.")

(defvar scad-font-lock-keywords
  `(
    ("\\(module\\|function\\)[ \t]+\\(\\sw+\\)" (1 'font-lock-keyword-face nil) (2 'font-lock-function-name-face nil t))
    ("<\\(\\sw+\\)>" (1 'font-lock-builtin-face nil))
    ("$\\(\\sw+\\)" (1 'font-lock-builtin-face nil))
    (,scad-keywords-regexp . font-lock-keyword-face)
    (,scad-modules-regexp .  font-lock-builtin-face)
    (,scad-functions-regexp .  font-lock-function-name-face)
    ;(,scad-operators-regexp .  font-lock-operator-face) ;; This actually looks pretty ugly
    ;("\\(\\<\\S +\\>\\)\\s *(" 1 font-lock-function-name-face t) ;; Seems to override other stuff (e.g. in comments and builtins)
    )
  "Keyword highlighting specification for `scad-mode'.")

(defvar scad-indent-style nil
  "The stlye indentation for scad-mode. Defaults to \"gnu\" if nil.")

(put 'scad-mode 'c-mode-prefix "scad-")
;;;###autoload
(define-derived-mode scad-mode prog-mode "AWK"
  "Major mode for editing OpenSCAD code.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `scad-mode-hook'.

Key bindings:
\\{scad-mode-map}"
  (c-initialize-cc-mode t)
  ;; (setq local-abbrev-table scad-mode-abbrev-table
  ;; 	abbrev-mode t)
  (use-local-map scad-mode-map)
  (c-init-language-vars scad-mode)
  ;; BUG: comments in cc-mode.el do not mention the requierd style
  ;; argument
  (c-basic-common-init 'scad-mode (or scad-indent-style "gnu"))
  ;; (c-awk-unstick-NL-prop)

  (c-run-mode-hooks 'c-mode-common-hook 'scad-mode-hook)
  (c-update-modeline))


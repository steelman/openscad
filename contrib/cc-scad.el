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

(require 'cc-mode)

(defcustom scad-keywords
  '("return" "true" "false")
  "SCAD keywords."
  :type 'list
  :group 'scad-font-lock)

(defcustom scad-functions
  '("cos" "acos" "sin" "asin" "tan" "atan" "atan2"                      ;;func.cc
    "abs" "sign" "rands" "min" "max"
    "round" "ceil" "floor"
    "pow" "sqrt" "exp" "log" "ln"
    "str"
    "lookup" "version" "version_num" "len" "search"
    "dxf_dim" "dxf_cross"                                               ;;dxfdim.cc
    )
  "SCAD functions."
  :type 'list
  :group 'scad-font-lock)

(defcustom scad-modules
  '("child" "echo" "assign" "for" "intersection_for" "if" "else"        ;;control.cc
    "cube" "sphere" "cylinder" "polyhedron" "square" "circle" "polygon" ;;primitives.cc
    "scale" "rotate" "translate" "mirror" "multmatrix"                  ;;transform.cc
    "union" "difference" "intersection"                                 ;;csgops.cc
    "render"                                                            ;;render.cc
    "color"                                                             ;;color.cc
    "surface"                                                           ;;surface.cc
    "dxf_linear_extrude" "linear_extrude"                               ;;linearextrude.cc
    "dxf_rotate_extrude" "rotate_extrude"                               ;;rotateextrude.cc
    "import_stl" "import_off" "import_dxf" "import"                     ;;import.cc
    "group"                                                             ;;builtin.cc
    "projection"                                                        ;;projection.cc
    "minkowski" "glide" "subdiv" "hull" "resize"                        ;;cgaladv.cc
    )
  "SCAD modules."
  :type 'list
  :group 'scad-font-lock)

(defvar scad-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control c) (control o)] 'scad-open-current-buffer)
    (define-key map [(control c) (control s)] 'c-show-syntactic-information)
    (define-key map [return] 'newline-and-indent)
    map)
  "Keymap for `scad-mode'.")

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

(defvar scad-keywords-regexp (regexp-opt scad-keywords 'words))
(defvar scad-modules-regexp (regexp-opt scad-modules 'words))
(defvar scad-functions-regexp (regexp-opt scad-functions 'words))

(defvar scad-font-lock-keywords
  `(
    ("\\(module\\|function\\)[ \t]+\\(\\sw+\\)" (1 'font-lock-keyword-face nil) (2 'font-lock-function-name-face nil t))
    ("<\\(\\sw+\\)>" (1 'font-lock-builtin-face nil))
    ("$\\(\\sw+\\)" (1 'font-lock-builtin-face nil))
    (,scad-keywords-regexp . font-lock-keyword-face)
    (,scad-modules-regexp .  font-lock-builtin-face)
    (,scad-functions-regexp .  font-lock-function-name-face))
  "Keyword highlighting specification for `scad-mode'.")
(defconst scad-font-lock-keywords-1 scad-font-lock-keywords)
(defconst scad-font-lock-keywords-2 scad-font-lock-keywords)
(defconst scad-font-lock-keywords-3 scad-font-lock-keywords)

(defvar scad-indent-style nil
  "The stlye indentation for scad-mode. Defaults to \"gnu\" if
  nil. If you want to set the style with file local variables use
  the `c-file-style' variable")

(put 'scad-mode 'c-mode-prefix "scad-")
;;;###autoload
(define-derived-mode scad-mode prog-mode "SCAD"
  "Major mode for editing OpenSCAD code.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `scad-mode-hook'.

Key bindings:
\\{scad-mode-map}"
  (c-initialize-cc-mode)
  ;; (setq local-abbrev-table scad-mode-abbrev-table
  ;; 	abbrev-mode t)
  (use-local-map scad-mode-map)
  ;(c-init-language-vars scad-mode)
  ;; BUG: comments in cc-mode.el do not mention the requierd style
  ;; argument
  (c-basic-common-init 'scad-mode (or scad-indent-style "gnu"))
  (c-font-lock-init)
  ;; (c-awk-unstick-NL-prop)

  (c-run-mode-hooks 'c-mode-common-hook 'scad-mode-hook)
  (c-update-modeline))


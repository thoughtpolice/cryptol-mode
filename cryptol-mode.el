;;; cryptol-mode.el --- Cryptol major mode for Emacs

;; Copyright (c) 2013 Austin Seipp. All rights reserved.

;; Author:    Austin Seipp <aseipp [@at] pobox [dot] com>
;; URL:       http://github.com/thoughtpolice/cryptol-mode
;; Keywords:  cryptol cryptography
;; Version:   0.0.0-DEV
;; Released:  11 Feburary 2013

;; This file is not part of GNU Emacs.

;;; License:

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;
;; [ MIT license: http://www.opensource.org/licenses/MIT ]

;;; Commentary:

;; This package provides a major mode for editing, compiling and
;; running Cryptol code.
;;
;; For more information about Cryptol, check out the homepage and
;; documentation: http://corp.galois.com/cryptol/
;;
;; For usage info, release notes and bugs, check the homepage.

;;; TODO:

;; * Indentation mode.
;;  - Maybe something like Haskell-mode?
;; * imenu support
;; * Better highlighting.
;;  - We really want to identify function names.
;; * Compilation mode.
;;  - Detect compiler features?
;; * Better REPL integration
;;  - Run 'genTests' or 'check' or 'exhaust' on given function/theorem
;;  - Automatically run batch-mode files
;; * Syntax mode for literate files

(require 'comint)
(require 'easymenu)
(require 'font-lock)
(require 'generic-x)

;;; -- Customization variables -------------------------------------------------

(defconst cryptol-mode-version "0.0.0-DEV"
  "The version of `cryptol-mode'.")

(defgroup cryptol nil
  "A Cryptol major mode."
  :group 'languages)

(defcustom cryptol-tab-width tab-width
  "The tab width to use when indenting."
  :type  'integer
  :group 'cryptol)

(defcustom cryptol-command "cryptol"
  "The Cryptol command to use for evaluating code."
  :type  'string
  :group 'cryptol)

(defcustom cryptol-args-repl '("-n")
  "The arguments to pass to `cryptol-command' when starting a REPL."
  :type  'list
  :group 'cryptol)

(defcustom cryptol-args-compile '("-b")
  "The arguments to pass to `cryptol-command' to compile a file."
  :type  'list
  :group 'cryptol)

(defcustom cryptol-compiled-buffer-name "*cryptol-compiled*"
  "The name of the scratch buffer for compiled Cryptol."
  :type  'string
  :group 'cryptol)

(defcustom cryptol-mode-hook nil
  "Hook called by `cryptol-mode'."
  :type  'hook
  :group 'cryptol)

(defvar cryptol-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") 'cryptol-repl)
    map)
  "Keymap for `cryptol-mode'.")

;;; -- Menu --------------------------------------------------------------------

(defun cryptol-version ()
  "Show the `cryptol-mode' version in the echo area."
  (interactive)
  (let ((cryptol-ver-out (car (process-lines cryptol-command "-v"))))
      (message (concat "cryptol-mode v" cryptol-mode-version
		       ", using " cryptol-ver-out))))

(easy-menu-define cryptol-mode-menu cryptol-mode-map
  "Menu for Cryptol mode"
  '("Cryptol"
    ["Start REPL" cryptol-repl]
    "---"
    ["Customize Cryptol group" (customize-group 'cryptol)]
    "---"
    ["Version info" cryptol-version]
    ))

;;; -- Commands ----------------------------------------------------------------

(defun make-repl-command (file)
  (append (list cryptol-command) cryptol-args-repl (list file)))

(defun cryptol-repl ()
  "Launch a Cryptol REPL using `cryptol-command' as an inferior executable."
  (interactive)
  (if (eq nil (buffer-file-name))
      (message "Please save the current buffer before using the REPL.")
    (unless (comint-check-proc "*CryptolREPL*")
       (set-buffer
	(apply 'make-comint "CryptolREPL"
	       "env" nil
	       (make-repl-command (buffer-file-name)))))
    (pop-to-buffer "*CryptolREPL*")))

;;; -- Syntax table ------------------------------------------------------------

(defvar cryptol-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for `cryptol-mode'.")

;;; -- Language Syntax ---------------------------------------------------------

(defvar cryptol-string-regexp "\"\\.\\*\\?")

(defvar cryptol-symbols-regexp
  (regexp-opt '( "[" "]" "," "{" "}" "@" "#")))

(defvar cryptol-symbols2-regexp
  (regexp-opt '( "|" "=>" "->" ":")))

(defvar cryptol-consts-regexp
  (regexp-opt '( "True" "False")))

(defvar cryptol-type-regexp
  (regexp-opt '( "Bit" "inf")))

(defvar cryptol-keywords-regexp
  (regexp-opt '("module" "theorem" "where" "let" "if" "else" "then") 'words))

(defvar cryptol-font-lock-defaults
  `((,cryptol-string-regexp   . font-lock-string-face)
    (,cryptol-symbols-regexp  . font-lock-builtin-face)
    (,cryptol-symbols2-regexp . font-lock-variable-name-face)
    (,cryptol-keywords-regexp . font-lock-keyword-face)
    (,cryptol-consts-regexp   . font-lock-constant-face)
    (,cryptol-type-regexp     . font-lock-type-face)
    ))

;;; -- Mode entry --------------------------------------------------------------

;; Major mode for cryptol code
;;;###autoload
(define-derived-mode cryptol-mode prog-mode "Cryptol"
  "Major mode for editing Cryptol files"

  ;; Syntax highlighting
  (setq font-lock-defaults '((cryptol-font-lock-defaults)))

  ;; Indentation, no tabs
  (set (make-local-variable 'tab-width) cryptol-tab-width)
  (setq indent-tabs-mode nil))
(provide 'cryptol-mode)

;; Major mode used for .scr files (batch files)
;;;###autoload
(define-generic-mode 'cryptol-batch-mode
  '("#")                         ;; comments start with #
  '("help" "let" "load" "print"  ;; keywords
    "browse" "cd" "quit" "set"
    "reload" "check" "genTests"
    "exhaust" "type" "version"
    "echo" "info" "config"
    "edit" "script" "trace"
    "showtr" "deltr" "autotrace"
    "compile" "fm" "isabelle"
    "translate" "equals" "sat"
    "safe")
  '((":" . 'font-lock-builtin)   ;; ':' is a builtin
    ("@" . 'font-lock-operator)) ;; '@' is an operator
  nil nil                        ;; autoload is set below
  "A mode for Cryptol batch files")

;;; -- Autoloading -------------------------------------------------------------

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cry$"  . cryptol-mode))
;;(add-to-list 'auto-mode-alist '("\\.lcry$" . literate-cryptol-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.scr$"  . cryptol-batch-mode))

;;; cryptol-mode.el ends here

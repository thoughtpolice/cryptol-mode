;; Copyright (c) 2013 Austin Seipp. All rights reserved.
;; 
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

;; Version:   0.0.0.0-DEV
;; Keywords:  Cryptol Emacs Major-mode
;; Author:    Austin Seipp <aseipp [@at] pobox [dot] com>
;; URL:       http://github.com/thoughtpolice/cryptol-mode

;; TODO:
;;  - Everything

(require 'comint)
(require 'easymenu)
(require 'font-lock)

;;; -- Customization variables -------------------------------------------------

(defconst cryptol-mode-version "0.0.0.0-DEV"
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

(defcustom cryptol-args-repl '("")
  "The arguments to pass to `cryptol-command' to start a REPL."
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
  "Hook called by `cryptol-mode'. Examples:

  TODO FIXME"
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
  (message (concat "cryptol-mode version " cryptol-mode-version)))

(easy-menu-define cryptol-mode-menu cryptol-mode-map
  "Menu for Cryptol mode"
  '("Cryptol"
    ["REPL" cryptol-repl]
    "---"
    ["Version" cryptol-version]
    ))

;;; -- Commands ----------------------------------------------------------------

(defun cryptol-repl ()
  "Launch a Cryptol REPL using `cryptol-command' as an inferior executable."
  (interactive)
  (unless (comint-check-proc "*CryptolREPL*")
    (set-buffer
     (apply 'make-comint "CryptolREPL"
	    "env" nil
	    (append (list cryptol-command) cryptol-args-repl))))
  (pop-to-buffer "*CryptolREPL*"))

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
  (regexp-opt '( "[" "]" ";" ":" "," "{" "}" "=>" "->" "@") 'words))

(defvar cryptol-keywords-regexp
  (regexp-opt '("where" "let" "if" "else" "then") 'words))

(defvar cryptol-font-lock-defaults
  `((,cryptol-string-regexp   . font-lock-string-face)
    (,cryptol-symbols-regexp  . font-lock-builtin-face)
    (,cryptol-keywords-regexp . font-lock-keyword-face)
    ))

;;; -- Mode entry --------------------------------------------------------------

(define-derived-mode cryptol-mode prog-mode "Cryptol"
  "Major mode for editing Cryptol files"
  
  ;; Syntax highlighting
  (setq font-lock-defaults '((cryptol-font-lock-defaults)))

  ;; Indentation, no tabs
  (set (make-local-variable 'tab-width) cryptol-tab-width)
  (setq indent-tabs-mode nil))

; El fin.
(provide 'cryptol-mode)

;;; -- Autoloading -------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.cry$"  . cryptol-mode))
(add-to-list 'auto-mode-alist '("\\.lcry$" . cryptol-mode))
(add-to-list 'auto-mode-alist '("\\.scr$"  . cryptol-mode))

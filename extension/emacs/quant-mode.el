;;; quant-mode.el --- Major mode for Quant programming language -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sigmapitech
;; Author: Sigmapitech
;; Keywords: languages
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; A major mode for editing Quant programming language files.
;; Provides syntax highlighting, indentation, and comment support.

;;; Code:

(defvar quant-mode-hook nil
  "Hook run when entering Quant mode.")

;; Syntax table
(defvar quant-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; C-style comments // ...
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    ;; Python-style comments # ...
    (modify-syntax-entry ?# "< b" st)
    ;; Strings
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "\"" st)
    ;; Operators
    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?- "." st)
    (modify-syntax-entry ?= "." st)
    (modify-syntax-entry ?% "." st)
    (modify-syntax-entry ?< "." st)
    (modify-syntax-entry ?> "." st)
    (modify-syntax-entry ?& "." st)
    (modify-syntax-entry ?| "." st)
    (modify-syntax-entry ?^ "." st)
    (modify-syntax-entry ?! "." st)
    (modify-syntax-entry ?~ "." st)
    st)
  "Syntax table for Quant mode.")

;; Keywords
(defconst quant-keywords
  '("fn" "return" "if" "else" "for" "while" "continue" "break" "import" "from" "const")
  "Keywords in Quant language.")

(defconst quant-types
  '("int" "float" "bool" "str" "void")
  "Type keywords in Quant language.")

(defconst quant-constants
  '("True" "False")
  "Constants in Quant language.")

;; Font lock
(defvar quant-font-lock-keywords
  (let ((keyword-regexp (regexp-opt quant-keywords 'words))
        (type-regexp (regexp-opt quant-types 'words))
        (constant-regexp (regexp-opt quant-constants 'words)))
    `(
      ;; Keywords
      (,keyword-regexp . font-lock-keyword-face)
      ;; Types
      (,type-regexp . font-lock-type-face)
      ;; Constants
      (,constant-regexp . font-lock-constant-face)
      ;; Function definitions
      ("\\<fn\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1 font-lock-function-name-face)
      ;; Function calls
      ("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*(" 1 font-lock-function-name-face)
      ;; Numbers (hex, octal, binary, decimal, float)
      ("\\<0[xX][0-9a-fA-F]+\\>" . font-lock-constant-face)
      ("\\<0[oO][0-7]+\\>" . font-lock-constant-face)
      ("\\<0[bB][01]+\\>" . font-lock-constant-face)
      ("\\<[0-9]+\\(\\.[0-9]+\\)?\\>" . font-lock-constant-face)
      ))
  "Keyword highlighting for Quant mode.")

;; Indentation
(defun quant-indent-line ()
  "Indent current line as Quant code."
  (interactive)
  (let ((indent-col 0)
        (cur-indent (current-indentation)))
    (save-excursion
      (beginning-of-line)
      (if (bobp)
          (setq indent-col 0)
        (let ((not-indented t))
          (if (looking-at "^[ \t]*}")
              (progn
                (save-excursion
                  (forward-line -1)
                  (setq indent-col (max 0 (- (current-indentation) 4))))
                (setq not-indented nil))
            (while not-indented
              (forward-line -1)
              (if (looking-at "^[ \t]*}")
                  (progn
                    (setq indent-col (current-indentation))
                    (setq not-indented nil))
                (if (looking-at ".*{[ \t]*$")
                    (progn
                      (setq indent-col (+ (current-indentation) 4))
                      (setq not-indented nil))
                  (if (bobp)
                      (setq not-indented nil)))))))))
    (if (= indent-col cur-indent)
        (if (< (current-column) indent-col)
            (move-to-column indent-col))
      (save-excursion
        (indent-line-to indent-col))
      (if (< (current-column) indent-col)
          (move-to-column indent-col)))))

;; Mode definition
;;;###autoload
(define-derived-mode quant-mode prog-mode "Quant"
  "Major mode for editing Quant programming language files."
  :syntax-table quant-mode-syntax-table
  
  ;; Comments
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "\\(?://+\\|#+\\)\\s-*")
  (setq-local comment-end "")
  
  ;; Font lock
  (setq-local font-lock-defaults '(quant-font-lock-keywords))
  
  ;; Indentation
  (setq-local indent-line-function 'quant-indent-line)
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode nil))

;; Auto-mode
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.qa\\'" . quant-mode))

(provide 'quant-mode)
;;; quant-mode.el ends here

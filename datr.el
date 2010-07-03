; datr-mode.el -- major mode for editing DATR files

;; Copyright (C) 2007 Sebastian Nagel ;;

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


(provide 'datr)

(defvar datr-mode-hook nil)

(defconst datr-version "0.01" "Version of datr-mode")


(setq auto-mode-alist ; files for which datr-mode will be invoked.
      (append '(("\\.dtr$" . datr-mode)
                ) auto-mode-alist))


(defvar datr-mode-syntax-table
  (let ((datr-mode-syntax-table (make-syntax-table)))
    ; comments are start with %
    (modify-syntax-entry ?%  "<" datr-mode-syntax-table)
    ; until end-of-line
    (modify-syntax-entry ?\n ">" datr-mode-syntax-table)
    (modify-syntax-entry ?\" "$" datr-mode-syntax-table)
    (modify-syntax-entry ?\\ "/" datr-mode-syntax-table)
    (modify-syntax-entry ?\' "\"" datr-mode-syntax-table)
    (modify-syntax-entry ?< "(>" datr-mode-syntax-table)
    (modify-syntax-entry ?> ")<" datr-mode-syntax-table)
    (modify-syntax-entry ?{ "." datr-mode-syntax-table)
    (modify-syntax-entry ?} "." datr-mode-syntax-table)
    (modify-syntax-entry ?( "." datr-mode-syntax-table)
    (modify-syntax-entry ?) "." datr-mode-syntax-table)
    (modify-syntax-entry ?[ "." datr-mode-syntax-table)
    (modify-syntax-entry ?] "." datr-mode-syntax-table)
    datr-mode-syntax-table)
  "Syntax table for datr-mode")


(defconst datr-font-lock-keywords
  (list
   ;; macros
   (list "^[[:space:]]*\\(#[[:space:]]*\\(?:seed\\|sort\\|atom\\|node\\)\\)[[:space:]]*\\(\\(?:'\\(?:\\\\'\\|[^']\\)*'\\|[^.]\\)+\\)\\."
         (list 1 'font-lock-preprocessor-face nil t)
         ;; (list 2 'font-lock-string-face nil t)
         )
   (list "^[[:space:]]*\\(#[[:space:]]*vars\\)[[:space:]]*\\(\\$\\w+\\)[[:space:]]*:[[:space:]]*\\(\\(?:'\\(?:\\\\'\\|[^']\\)*'\\|[^.]\\)+\\)\\."
         (list 1 'font-lock-preprocessor-face nil t)
         (list 2 'font-lock-variable-name-face nil t)
         ;; (list 3 'font-lock-string-face nil t)
         )
   (list "^[[:space:]]*\\(#[[:space:]]*\\(?:uses\\|load\\|include\\)\\)[[:space:]]*\\(\\(?:'\\(?:\\\\'\\|[^']\\)*'\\|[^.]\\)+\\)\\."
         (list 1 'font-lock-preprocessor-face nil t)
         (list 2 'font-lock-keyword-face nil t))
   (list "^[[:space:]]*\\(#[[:space:]]*\\(?:show\\(?:if\\)?\\|query\\|hide\\)\\)[[:space:]]*\\(\\(?:'\\(?:\\\\'\\|[^']\\)*'\\|[^.]\\)+\\)\\."
         (list 1 'font-lock-preprocessor-face nil t)
         ;; (list 2 'font-lock-builtin-face nil t)
         )
   ;; path definition
   (list "\\(<[^=>]*>\\)[[:space:]]*==?"
         (list 1 'font-lock-type-face nil t))
   ;; local/global pathes
   (list "\\(<[^<\">]*>\\|\"[[:space:]]*\\(?:<[^>]*>\\|[A-Z]\\w*\\)[[:space:]]*\"\\)"
         (list 1 'font-lock-constant-face nil t))
   ;; nodes
   (list "\\(\\<[A-Z]\\w*\\)"
         (list 1 'font-lock-function-name-face nil t))
   ;; variables
   (list "\\(\\$\\w+\\)"
         (list 1 'font-lock-variable-name-face nil t))
   ;; atoms
;;    (list "\\(\\<\\(?:\\w+\\|[^A-Z:<>'[:space:]][^:<>'[:space:]]*\\)\\|[\\\|'\\(?:\\\\'\\|[^']\\)*'\\)"
;;          (list 1 'font-lock-string-face nil t))
   ;; error
;;    (list "^\\(\\w+\\)$"
;;          (list 1 'font-lock-warning-face t))
   )
  "Expressions to highlight in datr-mode.")

(defun datr-font ()
  "Set font-lock variables for datr mode."
  (make-local-variable 'font-lock-keywords-case-fold-search) ; For GNU Emacs.
  (setq font-lock-keywords-case-fold-search nil)
  (put major-mode 'font-lock-keywords-case-fold-search nil) ; For XEmacs.
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(datr-font-lock-keywords nil nil)))


(defvar datr-mode-map
  (let ((datr-mode-map (make-sparse-keymap)))
    (define-key datr-mode-map "\t" 'datr-electric-tab)
    datr-mode-map)
  "Keymap for datr mode.")

(defconst datr-indent-level 2
  "*Indentation of datr statements with respect to containing block.")

(defconst datr-tab-always-indent t
  "*Non-nil means TAB in datr-mode should always reindent the 
   current line, regardless of where in the line point is when TAB
   is used.")

(defun datr-electric-tab ()
  "Function called when TAB is pressed in FSA-Grammar mode."
  (interactive)
  (let ((old-point (point-marker)))
    (save-excursion
      (beginning-of-line)
      (datr-indent-line))
    (beginning-of-line)
    (skip-chars-forward " \t")
    (if (> old-point (point))
	(goto-char old-point))))

(defconst datr-indent-plus "^[[:space:]]*<[^=>]*>[[:space:]]*==?")
(defconst datr-indent-begin-block "^[[:space:]]*[A-Z]\\w*:")
(defconst datr-indent-end-block "^[[:space:]]*\\.")

(defun datr-indent-line ()
  "Indent current line for FSA-Grammar mode."
  (interactive)
  (beginning-of-line)
  (if (bobp)  ; first line in buffer
      (indent-line-to 0)
    (if (looking-at datr-indent-begin-block)
        (indent-line-to 0)
      (if (looking-at datr-indent-end-block)
          (indent-line-to datr-indent-level)
        (if (looking-at datr-indent-plus)
            (indent-line-to datr-indent-level))))))

(defun datr-mode ()
  "Major mode for editing files DATR files."
  (interactive)
  (kill-all-local-variables)
  (use-local-map datr-mode-map)
  (setq major-mode 'datr-mode)
  (setq mode-name "DATR")
  (setq parse-sexp-ignore-comments t)
  (set-syntax-table datr-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'datr-indent-line)
  (make-local-variable 'comment-start)  
  (setq comment-start "% ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "% *")
  (make-local-variable 'completion-ignore-case)
  (setq completion-ignore-case nil)
  (datr-font)
  (run-hooks 'datr-mode-hook))





;;; doconce-mode.el --- Major-mode for the Doconce markup language.

;; Copyright (C) 2014 Lars Tveito.

;; Author: Lars Tveito <larstvei@ifi.uio.no>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major-mode for Doconce markup language.

;;; Code:
(defconst doconce-mode-version "0.1"
  "Doconce mode version number.")

(defvar doconce-mode-hook nil
  "Hook run when entering Doconce mode.")

(defgroup doconce nil
  "Major mode for the Doconce markup language."
  :prefix "doconce-"
  :group 'wp)

;;; Font lock

(require 'font-lock)

(defvar doconce-code-face 'doconce-code-face
  "Face name to use for italic text.")

(defvar doconce-inline-code-face 'doconce-inline-code-face
  "Face name to use for inline code.")

(defvar doconce-header-face 'doconce-header-face
  "Face name to use for level-1 headers.")

(defvar doconce-footnote-face 'doconce-footnote-face
  "Face name to use for footnote identifiers.")

(defvar doconce-link-face 'doconce-link-face
  "Face name to use for links.")

(defvar doconce-comment-face 'doconce-comment-face
  "Face name to use for comments.")

(defvar doconce-math-face 'doconce-math-face
  "Face name to use for LaTeX expressions.")

(defvar doconce-special-lines-face 'doconce-special-lines-face
  "Face name to use for special lines.")

(defvar doconce-admonition-face 'doconce-admonition-face
  "Face name to use for admonitions.")

(defgroup doconce-faces nil
  "Faces used in Doconce Mode"
  :group 'doconce
  :group 'faces)

(defface doconce-header-face
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Base face for headers."
  :group 'doconce-faces)

(defface doconce-code-face
  '((t (:inherit font-lock-constant-face)))
  "Face for inline code."
  :group 'doconce-faces)

(defface doconce-reference-face
  '((t (:inherit font-lock-type-face)))
  "Face for link references."
  :group 'doconce-faces)

(defface doconce-footnote-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for footnote markers."
  :group 'doconce-faces)

(defface doconce-link-face
  '((t (:inherit link)))
  "Face for links."
  :group 'doconce-faces)

(defface doconce-comment-face
  '((t (:inherit font-lock-comment-face)))
  "Face for Doconce comments."
  :group 'doconce-faces)

(defface doconce-math-face
  '((t (:inherit font-lock-string-face)))
  "Face for LaTeX expressions."
  :group 'doconce-faces)

(defface doconce-special-lines-face
  '((t (:inherit doconce-comment-face :underline t)))
  "Face for admonitions."
  :group 'doconce-faces)

(defface doconce-admonition-face
  '((t (:inherit doconce-header-face)))
  "Face for admonitions."
  :group 'doconce-faces)

(defconst doconce-regex-footnote
  "\\[\\^.+?\\]"
  "Regular expression for a footnote marker [^fn].")

(defconst doconce-regex-header
  "^==+ *.+ *==+$"
  "Regexp identifying Doconce headers.")

(defconst doconce-regex-admonitions
  "^__.+?[.:?]__"
  "Regexp identifying Doconce admonitions.")

(defconst doconce-regex-code
  "^!bc[^§]+?!ec"
  "Regular expression for matching code blocks.")

(defconst doconce-regex-code-file
  "^@@@CODE .+"
  "Regular expression for matching code from file.")

(defconst doconce-regex-inline-code
  "`.+?`"
  "Regular expression for matching inline code fragments.")

(defconst doconce-regex-email
  "<\\(\\(\\sw\\|\\s_\\|\\s.\\)+@\\(\\sw\\|\\s_\\|\\s.\\)+\\)>"
  "Regular expression for matching inline email addresses.")

(defconst doconce-regex-link
  "\\(\".*\"\\):[ \t\n]*\\(\".*\"\\)"
  "Regular expression for matching links.")

(defconst doconce-regex-comment
  "^#.+$"
  "Regular expression for matching comments.")

(defconst doconce-regex-math-inline
  "\\(^\\|[^\\]\\)\\(\\$\\($\\([^\\$]\\|\\\\.\\)*\\$\\|\\([^\\$]\\|\\\\.\\)*\\)\\$\\)"
  "Regular expression for inline LaTeX.")

(defconst doconce-regex-math-display
  "\\(^!bt\\([^§]+?\\)!et\\)"
  "Regular expression for LaTeX equations.")

(defconst doconce-regex-special-lines
  "\\(^\\(FIGURE\\|MOVIE\\|AUTHOR\\|TITLE\\|DATE\\|BIBFILE\\):.+$\\)"
  "Regular expression for special lines.")

(defconst doconce-regex-reference
  "ref\{.+?\}"
  "Regular expression for references.")

(defvar doconce-keywords
  '("!bwarning" "!ewarning" "!bquote" "!equote" "!bnotice" "!enotice"
  "!bsummary" "!esummary" "!bquestion" "!equestion" "!bblock" "!eblock"
  "!bbox" "!ebox" "!bsubex" "!esubex" "!bhint" "!ehint" "!bsol" "!esol"
  "!bans" "!eans" "!bremarks" "eremarks" "!bpop" "epop" "!bslidecell"
  "eslidecell" "idx" "TOC" "label" "cite"))

(defvar doconce-mode-font-lock-keywords-basic
  (list
   (cons doconce-regex-comment 'doconce-comment-face)
   (cons doconce-regex-header 'doconce-header-face)
   (cons doconce-regex-admonitions 'doconce-header-face)
   (cons doconce-regex-math-inline '(2 doconce-math-face))
   (cons doconce-regex-math-display '(1 doconce-header-face))
   (cons doconce-regex-inline-code 'doconce-code-face)
   (cons doconce-regex-code-file 'doconce-special-lines-face)
   (cons doconce-regex-email 'doconce-url-face)
   (cons doconce-regex-link '((1 doconce-link-face)
                              (2 doconce-link-face)))
   (cons doconce-regex-footnote 'doconce-footnote-face)
   (cons doconce-regex-special-lines '(1 doconce-special-lines-face)))
  "Syntax highlighting for Doconce files.")

(defun doconce-font-lock-extend-region ()
  "Extend the search region to include an entire block of text.
This helps improve font locking for block constructs such as pre blocks."
  ;; Avoid compiler warnings about these global variables from font-lock.el.
  ;; See the documentation for variable `font-lock-extend-region-functions'.
  (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
  (save-excursion
    (goto-char font-lock-beg)
    (unless (looking-back "\n\n")
      (let ((found (or (re-search-backward "\n\n" nil t) (point-min))))
        (goto-char font-lock-end)
        (when (re-search-forward "\n\n" nil t)
          (setq font-lock-end (match-beginning 0))
          (setq font-lock-beg found))))))

(defun doconce-reload-extensions ()
  "Check settings, update font-lock keywords, and re-fontify buffer."
  (interactive)
  (when (eq major-mode 'doconce-mode)
    (setq doconce-mode-font-lock-keywords
          (append
           doconce-mode-font-lock-keywords-basic
           doconce-keywords))
    (setq font-lock-defaults (list doconce-mode-font-lock-keywords))
    (font-lock-refresh-defaults)))

(define-derived-mode doconce-mode text-mode "Doconce"
  "Major mode for the Doconce markup language."

  (set (make-local-variable 'comment-start) "## ")
  (set (make-local-variable 'comment-start-skip) "##+\\s-*")

  (set (make-local-variable 'doconce-mode-font-lock-keywords) nil)
  (set (make-local-variable 'font-lock-defaults) nil)
  (set (make-local-variable 'font-lock-multiline) t)
  (doconce-reload-extensions)
  (add-hook 'font-lock-extend-region-functions
            'doconce-font-lock-extend-region))

;;; doconce-mode.el ends here

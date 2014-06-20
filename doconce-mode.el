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

(require 'font-lock)
(require 'outline)

(defconst doconce-mode-version "0.1"
  "Doconce mode version number.")

(defvar doconce-mode-hook nil
  "Hook run when entering Doconce mode.")

(defgroup doconce nil
  "Major mode for the Doconce markup language."
  :prefix "doconce-"
  :group 'wp)

;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.do.txt\\'") 'doconce-mode))

;;; Font lock

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
  "^\\(========= .* =========\\|======= .* =======\\|===== .* =====\\|=== .* ===\\)$"
  "Regexp identifying Doconce headers.")

(defconst doconce-regex-admonitions
  "^__.+?[.:?]__"
  "Regexp identifying Doconce admonitions.")

(defconst doconce-regex-code
  "\\(^!bc\\([^§]+?\\)!ec\\)"
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
  '("!bt" "!et" "!bc" "!ec" "!bwarning" "!ewarning" "!bquote" "!equote"
    "!bnotice" "!enotice" "!bsummary" "!esummary" "!bquestion" "!equestion"
    "!bblock" "!eblock" "!bbox" "!ebox" "!bsubex" "!esubex" "!bhint"
    "!ehint" "!bsol" "!esol" "!bans" "!eans" "!bremarks" "eremarks" "!bpop"
    "!epop" "!bslidecell" "!eslidecell" "idx" "TOC" "label" "cite"))

(defvar doconce-mode-font-lock-keywords-basic
  (list
   (cons doconce-regex-comment 'doconce-comment-face)
   (cons doconce-regex-header 'doconce-header-face)
   (cons doconce-regex-admonitions 'doconce-header-face)
   (cons doconce-regex-math-inline '(2 doconce-code-face))
   (cons doconce-regex-math-display '(2 doconce-code-face))
   (cons doconce-regex-code '(2 doconce-code-face))
   (cons doconce-regex-code-file 'doconce-special-lines-face)
   (cons doconce-regex-inline-code 'doconce-code-face)
   (cons doconce-regex-email 'doconce-url-face)
   (cons doconce-regex-link '((1 doconce-math-face)
                              (2 doconce-link-face)))
   (cons doconce-regex-footnote 'doconce-footnote-face)
   (cons doconce-regex-special-lines '(1 doconce-special-lines-face)))
  "Syntax highlighting for Doconce files.")

;;; Defuns:

(defun doconce-insert-link ()
  "An interactive function that inserts a link."
  (interactive)
  (let ((desc (read-string
               "Description: " nil 'minibuffer-history "description"))
        (link (read-string
               "Link: " nil 'minibuffer-history "http://"))
        (point (point)))
    (insert "\"" desc "\": \"")
    (insert-text-button link
                   'follow-link t
                   'action #'browse-url-at-point)
    (insert "\"")))

(defun doconce-insert-heading ()
  "An interactive function that inserts a Doconce heading."
  (interactive)
  (let (level)
    (save-excursion
      (setq level
            (condition-case nil
                (and (outline-back-to-heading t)
                     (doconce-outline-level))
              (error 2))))
    (unless (= (point-at-bol) (point-at-eol))
      (beginning-of-line) (open-line 1))
    (insert (car (rassoc level outline-heading-alist)))
    (search-backward "  " (point-at-bol) t)))

(defun doconce-outline-level ()
  "Function that takes no args to compute a header's nesting level in an
outline. It assumes the point is at the beginning of a header line and that
the match data reflects the `outline-regexp'."
  (save-excursion
    (let ((level-1 (make-string 9 ?=))
          (level-2 (make-string 7 ?=))
          (level-3 (make-string 5 ?=))
          (level-4 (make-string 3 ?=)))
      (or (and (search-forward level-1 (point-at-eol) t 2) 1)
          (and (search-forward level-2 (point-at-eol) t 2) 2)
          (and (search-forward level-3 (point-at-eol) t 2) 3)
          (and (search-forward level-4 (point-at-eol) t 2) 4)))))

(defun doconce-promote ()
  "Promote the current heading higher up the tree."
  (interactive)
  (outline-back-to-heading t)
  (let* ((head (buffer-substring-no-properties
                (point-at-bol) (point-at-eol)))
         (new-head (concat "==" head "==")))
    (when (string-match-p doconce-regex-header new-head)
      (delete-char (- (point-at-eol) (point-at-bol)))
      (insert new-head))))

(defun doconce-demote ()
  "Demote the current heading lower down the tree."
  (interactive)
  (outline-back-to-heading t)
  (let* ((head (buffer-substring-no-properties
                (point-at-bol) (point-at-eol)))
         (new-head (and (> (length head) 4) (substring head 2 -2))))
    (when (and new-head (string-match-p doconce-regex-header new-head))
      (delete-char (- (point-at-eol) (point-at-bol)))
      (insert new-head))))


(defun doconce-indent-line ()
  "Indent the current line using some heuristics.
If the _previous_ command was either `doconce-enter-key' or
`doconce-cycle', then we should cycle to the next
reasonable indentation position.  Otherwise, we could have been
called directly by `doconce-enter-key', by an initial call of
`doconce-cycle', or indirectly by `auto-fill-mode'.  In
these cases, indent to the default position.
Positions are calculated by `doconce-calc-indents'."
  (interactive)
  (let ((positions (doconce-calc-indents))
        (cur-pos (current-column)))
    (if (not (equal this-command 'doconce-cycle))
        (indent-line-to (car positions))
      (setq positions (sort (delete-dups positions) '<))
      (indent-line-to
       (doconce-indent-find-next-position cur-pos positions)))))

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

;;; Outline

(defvar doconce-cycle-global-status 1)
(defvar doconce-cycle-subtree-status nil)

(defun doconce-end-of-subtree (&optional invisible-OK)
  "Move to the end of the current subtree.
Only visible heading lines are considered, unless INVISIBLE-OK is
non-nil.
Derived from `org-end-of-subtree'."
  (outline-back-to-heading invisible-OK)
  (let ((first t)
        (level (funcall outline-level)))
    (while (and (not (eobp))
                (or first (> (funcall outline-level) level)))
      (setq first nil)
      (outline-next-heading))
    (if (memq (preceding-char) '(?\n ?\^M))
        (progn
          ;; Go to end of line before heading
          (forward-char -1)
          (if (memq (preceding-char) '(?\n ?\^M))
              ;; leave blank line before heading
              (forward-char -1)))))
  (point))

(defun doconce-cycle (&optional arg)
  "Visibility cycling for Doconce mode.
If ARG is t, perform global visibility cycling.  If the point is
at an atx-style header, cycle visibility of the corresponding
subtree.  Otherwise, insert a tab using `indent-relative'.
Derived from `org-cycle'."
  (interactive "P")
  (cond
   ((eq arg t) ;; Global cycling
    (cond
     ((and (eq last-command this-command)
           (eq doconce-cycle-global-status 2))
      ;; Move from overview to contents
      (hide-sublevels 1)
      (message "CONTENTS")
      (setq doconce-cycle-global-status 3))

     ((and (eq last-command this-command)
           (eq doconce-cycle-global-status 3))
      ;; Move from contents to all
      (show-all)
      (message "SHOW ALL")
      (setq doconce-cycle-global-status 1))

     (t
      ;; Defaults to overview
      (hide-body)
      (message "OVERVIEW")
      (setq doconce-cycle-global-status 2))))

   ((save-excursion (beginning-of-line 1) (looking-at outline-regexp))
    ;; At a heading: rotate between three different views
    (outline-back-to-heading)
    (let ((goal-column 0) eoh eol eos)
      ;; Determine boundaries
      (save-excursion
        (outline-back-to-heading)
        (save-excursion
          (beginning-of-line 2)
          (while (and (not (eobp)) ;; this is like `next-line'
                      (get-char-property (1- (point)) 'invisible))
            (beginning-of-line 2)) (setq eol (point)))
        (outline-end-of-heading)   (setq eoh (point))
        (doconce-end-of-subtree t)
        (skip-chars-forward " \t\n")
        (beginning-of-line 1) ; in case this is an item
        (setq eos (1- (point))))
      ;; Find out what to do next and set `this-command'
      (cond
       ((= eos eoh)
        ;; Nothing is hidden behind this heading
        (message "EMPTY ENTRY")
        (setq doconce-cycle-subtree-status nil))
       ((>= eol eos)
        ;; Entire subtree is hidden in one line: open it
        (show-entry)
        (show-children)
        (message "CHILDREN")
        (setq doconce-cycle-subtree-status 'children))
       ((and (eq last-command this-command)
             (eq doconce-cycle-subtree-status 'children))
        ;; We just showed the children, now show everything.
        (show-subtree)
        (message "SUBTREE")
        (setq doconce-cycle-subtree-status 'subtree))
       (t
        ;; Default action: hide the subtree.
        (hide-subtree)
        (message "FOLDED")
        (setq doconce-cycle-subtree-status 'folded)))))

   (t
    (indent-for-tab-command))))

(defun doconce-shifttab ()
  "Global visibility cycling.
Calls `doconce-cycle' with argument t."
  (interactive)
  (doconce-cycle t))

(defvar doconce-mode-map
  (let ((map (make-keymap)))
    ;; Visibility cycling
    (define-key map (kbd "<tab>") 'doconce-cycle)
    (define-key map (kbd "<S-iso-lefttab>") 'doconce-shifttab)
    (define-key map (kbd "<S-tab>")  'doconce-shifttab)
    (define-key map (kbd "<backtab>") 'doconce-shifttab)

    (define-key map (kbd "<M-right>") 'doconce-promote)
    (define-key map (kbd "<M-left>")  'doconce-demote)

    (define-key map (kbd "C-c C-l") 'doconce-insert-link)

    (define-key map [remap outline-insert-heading] 'doconce-insert-heading)
    (define-key map [remap outline-promote] 'doconce-promote)
    (define-key map [remap outline-demote] 'doconce-demote)

    (define-key map [follow-link] 'mouse-face)

    map)
  "Keymap for Doconce major mode.")

(define-derived-mode doconce-mode outline-mode "Doconce"
  "Major mode for the Doconce markup language."

  (set (make-local-variable 'comment-start) "## ")
  (set (make-local-variable 'comment-start-skip) "##+\\s-*")

  (set (make-local-variable 'doconce-mode-font-lock-keywords) nil)
  (set (make-local-variable 'font-lock-defaults) nil)
  (set (make-local-variable 'font-lock-multiline) t)

  (set (make-local-variable 'outline-regexp) doconce-regex-header)
  (set (make-local-variable 'outline-level) 'doconce-outline-level)
  (setq outline-heading-alist '(("=======   =======" . 2)
                                ("=====   =====" . 3)
                                ("===   ===" . 4)
                                ("=========   =========" . 1)))
  (doconce-reload-extensions)
  (add-hook 'font-lock-extend-region-functions
            'doconce-font-lock-extend-region)

  )

(provide 'doconce-mode)

;;; doconce-mode.el ends here

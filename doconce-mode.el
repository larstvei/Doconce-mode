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

(define-derived-mode markdown-mode text-mode "Doconce"
  "Major mode for the Doconce markup language.")
;;; doconce-mode.el ends here

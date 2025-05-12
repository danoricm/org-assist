;;; org-assist-markup-rich-contents.el --- Org Assist: Markup and rich content helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2025 danoricm
;;
;; Author: danoricm
;; Maintainer: danoricm
;; Created: May 12, 2025
;; Modified: May 12, 2025
;; Version: 1.0.0
;; License: GNU Public License version 3
;; Homepage: https://github.com/danoricm/org-assist
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:
;; This module provides helpers for inserting emphasis, symbols, LaTeX, and rich content
;; formatting in Org-mode documents.

;;; Code:

(require 'org)

(defvar org-assist-markup-rich-contents-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "*") #'org-assist-bold)
    (define-key map (kbd "/") #'org-assist-italic)
    (define-key map (kbd "_") #'org-assist-underline)
    (define-key map (kbd "=\") #'org-assist-verbatim)
    (define-key map (kbd "~") #'org-assist-code)
    (define-key map (kbd "$") #'org-assist-latex-fragment)
    map)
  "Keymap for `org-assist-markup-rich-contents-mode'.")

;;;###autoload
(define-minor-mode org-assist-markup-rich-contents-mode
  "Minor mode for Org markup and content formatting helpers."
  :lighter " OrgMarkup"
  :keymap org-assist-markup-rich-contents-map)

(defun org-assist--wrap-region-or-insert (open close)
  "Wrap active region or insert OPEN and CLOSE pair."
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (save-excursion
          (goto-char end)
          (insert close)
          (goto-char beg)
          (insert open)))
    (insert open close)
    (backward-char (length close))))

(defun org-assist-bold ()
  "Insert or wrap bold markup."
  (interactive)
  (org-assist--wrap-region-or-insert "*" "*"))

(defun org-assist-italic ()
  "Insert or wrap italic markup."
  (interactive)
  (org-assist--wrap-region-or-insert "/" "/"))

(defun org-assist-underline ()
  "Insert or wrap underline markup."
  (interactive)
  (org-assist--wrap-region-or-insert "_" "_"))

(defun org-assist-verbatim ()
  "Insert or wrap verbatim markup."
  (interactive)
  (org-assist--wrap-region-or-insert "=" "="))

(defun org-assist-code ()
  "Insert or wrap code markup."
  (interactive)
  (org-assist--wrap-region-or-insert "~" "~"))

(defun org-assist-latex-fragment ()
  "Insert or wrap LaTeX math fragment."
  (interactive)
  (org-assist--wrap-region-or-insert "$" "$"))

(provide 'org-assist-markup-rich-contents)

;;; org-assist-markup-rich-contents.el ends here

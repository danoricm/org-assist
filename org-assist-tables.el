;;; org-assist-tables.el --- Org Assist: Table helpers -*- lexical-binding: t; -*-

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
;; This module provides assistive functions for working with Org-mode tables,
;; including quick table creation, alignment, formula insertion, and navigation.

;;; Code:

(require 'org)

(defvar org-assist-tables-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'org-assist-create-table)
    (define-key map (kbd "a") #'org-assist-align-table)
    (define-key map (kbd "f") #'org-assist-insert-formula)
    (define-key map (kbd "n") #'org-assist-next-table-field)
    (define-key map (kbd "p") #'org-assist-previous-table-field)
    map)
  "Keymap for `org-assist-tables-mode'.")

;;;###autoload
(define-minor-mode org-assist-tables-mode
  "Minor mode for Org table editing helpers."
  :lighter " OrgTbls"
  :keymap org-assist-tables-map)

(defun org-assist-tables-create-table (rows cols)
  "Create a new Org table with ROWS and COLS."
  (interactive "nRows: \nnCols: ")
  (dotimes (_ rows)
    (insert (mapconcat (lambda (_) "|") (make-list (1+ cols) nil) "") "|\n"))
  (forward-line (- rows)))

(defun org-assist-tables-align-table ()
  "Align the current Org table."
  (interactive)
  (when (org-at-table-p)
    (org-table-align)))

(defun org-assist-tables-insert-formula ()
  "Prompt and insert a formula for current table."
  (interactive)
  (let ((formula (read-string "Formula (e.g., $3=$1+$2): ")))
    (save-excursion
      (goto-char (org-table-end))
      (insert "#+TBLFM: " formula "\n"))))

(defun org-assist-tables-next-table-field ()
  "Move to the next table field."
  (interactive)
  (when (org-at-table-p)
    (org-table-next-field)))

(defun org-assist-tables-previous-table-field ()
  "Move to the previous table field."
  (interactive)
  (when (org-at-table-p)
    (org-table-previous-field)))

(provide 'org-assist-tables)

;;; org-assist-tables.el ends here
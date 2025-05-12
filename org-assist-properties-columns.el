;;; org-assist-properties-columns.el --- Org Assist: Properties and column view helpers -*- lexical-binding: t; -*-

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
;; This module simplifies working with Org-mode properties and column views,
;; making it easier to insert, edit, and manage property drawers and column formats.

;;; Code:

(require 'org)

(defvar org-assist-properties-columns-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") #'org-assist-insert-property)
    (define-key map (kbd "d") #'org-assist-delete-property)
    (define-key map (kbd "P") #'org-assist-insert-properties-drawer)
    (define-key map (kbd "c") #'org-assist-set-column-view)
    map)
  "Keymap for `org-assist-properties-columns-mode'.")

;;;###autoload
(define-minor-mode org-assist-properties-columns-mode
  "Minor mode for Org property and column helpers."
  :lighter " OrgProps"
  :keymap org-assist-properties-columns-map)

(defun org-assist-insert-property (key value)
  "Insert or update a property with KEY and VALUE."
  (interactive "sProperty key: \nsProperty value: ")
  (org-set-property key value))

(defun org-assist-delete-property (key)
  "Delete property with KEY from the current entry."
  (interactive "sProperty key to delete: ")
  (org-delete-property key))

(defun org-assist-insert-properties-drawer ()
  "Insert an empty properties drawer."
  (interactive)
  (insert ":PROPERTIES:\n:END:\n")
  (forward-line -1))

(defun org-assist-set-column-view (format)
  "Set the column view format string."
  (interactive "sColumn format (e.g., %50ITEM(Task) %10Effort(Effort)): ")
  (insert (format "#+COLUMNS: %s\n" format)))

(provide 'org-assist-properties-columns)

;;; org-assist-properties-columns.el ends here

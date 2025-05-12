;;; org-assist-tags.el --- Org Assist: Tag helpers -*- lexical-binding: t; -*-

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
;; This module provides interactive helpers for inserting and managing Org-mode tags,
;; including tag completion, inheritance toggling, and tag search.

;;; Code:

(require 'org)

(defvar org-assist-tags-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'org-assist-add-tag)
    (define-key map (kbd "r") #'org-assist-remove-tag)
    (define-key map (kbd "t") #'org-assist-toggle-inheritance)
    (define-key map (kbd "s") #'org-assist-search-by-tag)
    map)
  "Keymap for `org-assist-tags-mode'.")

;;;###autoload
(define-minor-mode org-assist-tags-mode
  "Minor mode for Org tag management helpers."
  :lighter " OrgTags"
  :keymap org-assist-tags-map)

(defun org-assist-tags-add-tag (tag)
  "Add TAG to the current heading."
  (interactive "sTag to add: ")
  (org-set-tags-to (cons tag (org-get-tags))))

(defun org-assist-tags-remove-tag (tag)
  "Remove TAG from the current heading."
  (interactive "sTag to remove: ")
  (let ((tags (remove tag (org-get-tags))))
    (org-set-tags-to tags)))

(defun org-assist-tags-toggle-inheritance ()
  "Toggle tag inheritance for this buffer."
  (interactive)
  (setq org-use-tag-inheritance (not org-use-tag-inheritance))
  (message "Tag inheritance %s" (if org-use-tag-inheritance "enabled" "disabled")))

(defun org-assist-tags-search-by-tag (tag)
  "Search for TAG in buffer using Org sparse tree."
  (interactive "sSearch tag: ")
  (org-match-sparse-tree nil tag))

(provide 'org-assist-tags)

;;; org-assist-tags.el ends here
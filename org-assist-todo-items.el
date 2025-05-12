;;; org-assist-todo-items.el --- Org Assist: TODO helpers -*- lexical-binding: t; -*-

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
;; This module provides commands for creating and managing Org-mode TODO items,
;; including cycling states, priorities, and inserting checkboxes.

;;; Code:

(require 'org)

(defvar org-assist-todo-items-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") #'org-assist-toggle-todo)
    (define-key map (kbd "c") #'org-assist-insert-checkbox)
    (define-key map (kbd "x") #'org-assist-toggle-checkbox)
    (define-key map (kbd "p") #'org-assist-set-priority)
    map)
  "Keymap for `org-assist-todo-items-mode'.")

;;;###autoload
(define-minor-mode org-assist-todo-items-mode
  "Minor mode for Org TODO management helpers."
  :lighter " OrgTODO"
  :keymap org-assist-todo-items-map)

(defun org-assist-todo-items-toggle-todo ()
  "Toggle the TODO state at point."
  (interactive)
  (org-todo))

(defun org-assist-todo-items-insert-checkbox ()
  "Insert a new checkbox item."
  (interactive)
  (insert "- [ ] "))

(defun org-assist-todo-items-toggle-checkbox ()
  "Toggle the checkbox at point."
  (interactive)
  (org-toggle-checkbox))

(defun org-assist-todo-items-set-priority (priority)
  "Set priority of the current TODO heading."
  (interactive "cPriority (A, B, or C): ")
  (org-priority (upcase priority)))

(provide 'org-assist-todo-items)

;;; org-assist-todo-items.el ends here
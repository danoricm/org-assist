;;; org-assist-agenda-views.el --- Org Assist: Agenda and custom views helpers -*- lexical-binding: t; -*-

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
;; This module provides quick access to Org agenda features and
;; simplifies launching predefined or custom agenda commands.

;;; Code:

(require 'org)
(require 'org-agenda)

(defvar org-assist-agenda-views-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'org-assist-open-agenda)
    (define-key map (kbd "c") #'org-assist-custom-agenda)
    (define-key map (kbd "w") #'org-assist-week-view)
    (define-key map (kbd "d") #'org-assist-day-view)
    map)
  "Keymap for `org-assist-agenda-views-mode'.")

;;;###autoload
(define-minor-mode org-assist-agenda-views-mode
  "Minor mode for Org agenda view helpers."
  :lighter " OrgAgenda"
  :keymap org-assist-agenda-views-map)

(defun org-assist-open-agenda ()
  "Open the default Org agenda."
  (interactive)
  (org-agenda nil "a"))

(defun org-assist-custom-agenda ()
  "Prompt and run a custom agenda command."
  (interactive)
  (let ((key (read-string "Custom agenda key: ")))
    (org-agenda nil key)))

(defun org-assist-week-view ()
  "Open the agenda in weekly view."
  (interactive)
  (org-agenda-list nil nil 7))

(defun org-assist-day-view ()
  "Open the agenda in daily view."
  (interactive)
  (org-agenda-list nil nil 1))

(provide 'org-assist-agenda-views)

;;; org-assist-agenda-views.el ends here

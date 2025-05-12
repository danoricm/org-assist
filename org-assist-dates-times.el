;;; org-assist-dates-times.el --- Org Assist: Date and time helpers -*- lexical-binding: t; -*-

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
;; This module provides commands to assist with inserting and managing
;; timestamps, deadlines, scheduling, and clocking in Org-mode.

;;; Code:

(require 'org)

(defvar org-assist-dates-times-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") #'org-assist-insert-deadline)
    (define-key map (kbd "s") #'org-assist-insert-scheduled)
    (define-key map (kbd "t") #'org-assist-insert-timestamp)
    (define-key map (kbd "i") #'org-assist-clock-in)
    (define-key map (kbd "o") #'org-assist-clock-out)
    map)
  "Keymap for `org-assist-dates-times-mode'.")

;;;###autoload
(define-minor-mode org-assist-dates-times-mode
  "Minor mode for Org date/time helpers."
  :lighter " OrgTime"
  :keymap org-assist-dates-times-map)

(defun org-assist-insert-timestamp ()
  "Insert an active timestamp."
  (interactive)
  (org-insert-time-stamp (current-time) t t))

(defun org-assist-insert-deadline ()
  "Insert a DEADLINE timestamp."
  (interactive)
  (org-deadline nil (org-read-date)))

(defun org-assist-insert-scheduled ()
  "Insert a SCHEDULED timestamp."
  (interactive)
  (org-schedule nil (org-read-date)))

(defun org-assist-clock-in ()
  "Clock in to the current task."
  (interactive)
  (org-clock-in))

(defun org-assist-clock-out ()
  "Clock out of the current task."
  (interactive)
  (org-clock-out))

(provide 'org-assist-dates-times)

;;; org-assist-dates-times.el ends here

;;; org-assist-capture-attachments.el --- Org Assist: Capture and attachment helpers -*- lexical-binding: t; -*-

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
;; This module simplifies Org-mode capture and attachments,
;; providing quick helpers for launching capture templates and managing file attachments.

;;; Code:

(require 'org)
(require 'org-capture)
(require 'org-attach)

(defvar org-assist-capture-attachments-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'org-assist-capture)
    (define-key map (kbd "a") #'org-assist-attach-file)
    (define-key map (kbd "o") #'org-assist-open-attachment)
    map)
  "Keymap for `org-assist-capture-attachments-mode'.")

;;;###autoload
(define-minor-mode org-assist-capture-attachments-mode
  "Minor mode for Org capture and attachment helpers."
  :lighter " OrgCap"
  :keymap org-assist-capture-attachments-map)

(defun org-assist-capture ()
  "Launch Org capture."
  (interactive)
  (org-capture))

(defun org-assist-attach-file (file)
  "Attach FILE to current Org entry."
  (interactive "fFile to attach: ")
  (org-attach-attach file nil 'cp))

(defun org-assist-open-attachment ()
  "Open the attachment directory for current entry."
  (interactive)
  (org-attach-open))

(provide 'org-assist-capture-attachments)

;;; org-assist-capture-attachments.el ends here

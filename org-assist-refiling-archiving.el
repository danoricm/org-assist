;;; org-assist-refiling-archiving.el --- Org Assist: Refiling and archiving helpers -*- lexical-binding: t; -*-

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
;; This module provides helper functions to quickly refile or archive Org entries.

;;; Code:

(require 'org)

(defvar org-assist-refiling-archiving-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") #'org-assist-refile)
    (define-key map (kbd "a") #'org-assist-archive)
    (define-key map (kbd "s") #'org-assist-archive-subtree)
    map)
  "Keymap for `org-assist-refiling-archiving-mode'.")

;;;###autoload
(define-minor-mode org-assist-refiling-archiving-mode
  "Minor mode for Org refile/archive helpers."
  :lighter " OrgRefile"
  :keymap org-assist-refiling-archiving-map)

(defun org-assist-refiling-archiving-refile ()
  "Refile the current subtree using Org's refile interface."
  (interactive)
  (org-refile))

(defun org-assist-refiling-archiving-archive ()
  "Archive the current subtree using default method."
  (interactive)
  (org-archive-set-tag))

(defun org-assist-refiling-archiving-archive-subtree ()
  "Move the current subtree to the archive file."
  (interactive)
  (org-archive-subtree-default))

(provide 'org-assist-refiling-archiving)

;;; org-assist-refiling-archiving.el ends here
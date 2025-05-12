;;; org-assist-publishing.el --- Org Assist: Publishing helpers -*- lexical-binding: t; -*-

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
;; This module provides helper functions to configure and use Org-mode's
;; publishing system, including project definitions and execution.

;;; Code:

(require 'org)
(require 'ox-publish)

(defvar org-assist-publishing-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") #'org-assist-publish-project)
    (define-key map (kbd "a") #'org-assist-publish-all)
    (define-key map (kbd "e") #'org-assist-edit-project-alist)
    map)
  "Keymap for `org-assist-publishing-mode'.")

;;;###autoload
(define-minor-mode org-assist-publishing-mode
  "Minor mode for Org publishing helpers."
  :lighter " OrgPub"
  :keymap org-assist-publishing-map)

(defun org-assist-publish-project (project)
  "Publish the given PROJECT using Org publishing."
  (interactive
   (list (completing-read "Publish project: " (mapcar #'car org-publish-project-alist))))
  (org-publish project t))

(defun org-assist-publish-all ()
  "Publish all projects defined in `org-publish-project-alist'."
  (interactive)
  (org-publish-all t))

(defun org-assist-edit-project-alist ()
  "Open your init file or config file to edit `org-publish-project-alist'."
  (interactive)
  (find-file user-init-file)
  (goto-char (point-min))
  (search-forward "org-publish-project-alist" nil t))

(provide 'org-assist-publishing)

;;; org-assist-publishing.el ends here

;;; org-assist-hyperlinks.el --- Org Assist: Hyperlink helpers -*- lexical-binding: t; -*-

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
;; This module provides assistive commands for inserting and navigating Org-mode
;; hyperlinks more easily.

;;; Code:

(require 'org)

(defvar org-assist-hyperlinks-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") #'org-assist-insert-link)
    (define-key map (kbd "f") #'org-assist-follow-link-at-point)
    (define-key map (kbd "s") #'org-assist-store-link)
    map)
  "Keymap for `org-assist-hyperlinks-mode'.")

;;;###autoload
(define-minor-mode org-assist-hyperlinks-mode
  "Minor mode for Org hyperlink editing helpers."
  :lighter " OrgLinks"
  :keymap org-assist-hyperlinks-map)

(defun org-assist-insert-link (link desc)
  "Insert an Org hyperlink with LINK and DESC."
  (interactive "sLink (URL or file): \nsDescription: ")
  (insert (format "[[%s][%s]]" link desc)))

(defun org-assist-follow-link-at-point ()
  "Follow the Org link at point."
  (interactive)
  (org-open-at-point))

(defun org-assist-store-link ()
  "Store a link to the current location."
  (interactive)
  (org-store-link))

(provide 'org-assist-hyperlinks)

;;; org-assist-hyperlinks.el ends here

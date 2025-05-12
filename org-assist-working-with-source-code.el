;;; org-assist-working-with-source-code.el --- Org Assist: Source block helpers -*- lexical-binding: t; -*-

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
;; This module provides utilities for inserting and managing Org-mode source blocks,
;; editing them, evaluating them, and displaying results.

;;; Code:

(require 'org)
(require 'ob)

(defvar org-assist-working-with-source-code-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") #'org-assist-insert-src-block)
    (define-key map (kbd "e") #'org-assist-edit-src-block)
    (define-key map (kbd "x") #'org-assist-execute-src-block)
    (define-key map (kbd "r") #'org-assist-remove-results)
    map)
  "Keymap for `org-assist-working-with-source-code-mode'.")

;;;###autoload
(define-minor-mode org-assist-working-with-source-code-mode
  "Minor mode for Org source code block helpers."
  :lighter " OrgSrc"
  :keymap org-assist-working-with-source-code-map)

(defun org-assist-working-with-source-code-insert-src-block (lang)
  "Insert a source block for LANG."
  (interactive "sLanguage: ")
  (insert (format "#+begin_src %s\n\n#+end_src\n" lang))
  (forward-line -1))

(defun org-assist-working-with-source-code-edit-src-block ()
  "Open the current source block in its editing buffer."
  (interactive)
  (org-edit-special))

(defun org-assist-working-with-source-code-execute-src-block ()
  "Execute the current source block."
  (interactive)
  (org-babel-execute-src-block))

(defun org-assist-working-with-source-code-remove-results ()
  "Remove results of the current source block."
  (interactive)
  (org-babel-remove-result))

(provide 'org-assist-working-with-source-code)

;;; org-assist-working-with-source-code.el ends here
;;; org-assist-citation-handling.el --- Org Assist: Citation management helpers -*- lexical-binding: t; -*-

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
;; This module provides helper commands for inserting and working with citations
;; in Org-mode using `org-cite'. It also includes CSL style handling.

;;; Code:

(require 'oc)

(defvar org-assist-citation-handling-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") #'org-assist-insert-citation)
    (define-key map (kbd "s") #'org-assist-set-citation-style)
    (define-key map (kbd "b") #'org-assist-open-bibliography)
    map)
  "Keymap for `org-assist-citation-handling-mode'.")

;;;###autoload
(define-minor-mode org-assist-citation-handling-mode
  "Minor mode for Org citation helpers."
  :lighter " OrgCite"
  :keymap org-assist-citation-handling-map)

(defun org-assist-insert-citation (key)
  "Insert a citation with a given KEY."
  (interactive "sCitation key: ")
  (insert (format "[cite:@%s]" key)))

(defun org-assist-set-citation-style (style)
  "Set the citation export STYLE (CSL format)."
  (interactive "fCSL style file: ")
  (setq org-cite-csl-style (expand-file-name style))
  (message "Citation style set to: %s" org-cite-csl-style))

(defun org-assist-open-bibliography ()
  "Open the default bibliography file."
  (interactive)
  (find-file (car org-cite-global-bibliography)))

(provide 'org-assist-citation-handling)

;;; org-assist-citation-handling.el ends here

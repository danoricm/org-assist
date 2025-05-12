;;; org-assist-exporting.el --- Org Assist: Export helpers -*- lexical-binding: t; -*-

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
;; This module provides helper commands to export Org-mode content
;; to various formats (HTML, LaTeX, PDF, Markdown, etc.).

;;; Code:

(require 'org)
(require 'ox)

(defvar org-assist-exporting-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "h") #'org-assist-export-html)
    (define-key map (kbd "l") #'org-assist-export-latex)
    (define-key map (kbd "p") #'org-assist-export-pdf)
    (define-key map (kbd "m") #'org-assist-export-markdown)
    (define-key map (kbd "a") #'org-assist-export-async)
    map)
  "Keymap for `org-assist-exporting-mode'.")

;;;###autoload
(define-minor-mode org-assist-exporting-mode
  "Minor mode for Org export helpers."
  :lighter " OrgExport"
  :keymap org-assist-exporting-map)

(defun org-assist-exporting-export-html ()
  "Export the current buffer to HTML."
  (interactive)
  (org-html-export-to-html))

(defun org-assist-exporting-export-latex ()
  "Export the current buffer to LaTeX."
  (interactive)
  (org-latex-export-to-latex))

(defun org-assist-exporting-export-pdf ()
  "Export the current buffer to PDF via LaTeX."
  (interactive)
  (org-latex-export-to-pdf))

(defun org-assist-exporting-export-markdown ()
  "Export the current buffer to Markdown."
  (interactive)
  (org-export-to-file 'md (concat (file-name-sans-extension (buffer-file-name)) ".md")))

(defun org-assist-exporting-export-async ()
  "Prompt for export backend and export asynchronously."
  (interactive)
  (let ((backend (intern (completing-read "Export backend: " '("html" "latex" "md" "ascii")))))
    (org-export-to-file backend
      (concat (file-name-sans-extension (buffer-file-name)) "." (symbol-name backend)) nil nil nil t)))

(provide 'org-assist-exporting)

;;; org-assist-exporting.el ends here
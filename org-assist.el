;;; org-assist.el --- Org Assist: Unified minor mode for Org-mode helpers -*- lexical-binding: t; -*-

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
;; This is the main loader file for the Org Assist package, a modular system
;; that provides convenient helpers for editing and managing Org-mode documents.

;;; Code:

(require 'org-assist-document-structure)
(require 'org-assist-tables)
(require 'org-assist-hyperlinks)
(require 'org-assist-todo-items)
(require 'org-assist-tags)
(require 'org-assist-properties-columns)
(require 'org-assist-dates-times)
(require 'org-assist-refiling-archiving)
(require 'org-assist-capture-attachments)
(require 'org-assist-agenda-views)
(require 'org-assist-markup-rich-contents)
(require 'org-assist-exporting)
(require 'org-assist-publishing)
(require 'org-assist-citation-handling)
(require 'org-assist-working-with-source-code)

;;;###autoload
(define-minor-mode org-assist-mode
  "Enable all Org Assist minor modes at once."
  :lighter " OrgAssist"
  (if org-assist-mode
      (progn
        (org-assist-document-structure-mode 1)
        (org-assist-tables-mode 1)
        (org-assist-hyperlinks-mode 1)
        (org-assist-todo-items-mode 1)
        (org-assist-tags-mode 1)
        (org-assist-properties-columns-mode 1)
        (org-assist-dates-times-mode 1)
        (org-assist-refiling-archiving-mode 1)
        (org-assist-capture-attachments-mode 1)
        (org-assist-agenda-views-mode 1)
        (org-assist-markup-rich-contents-mode 1)
        (org-assist-exporting-mode 1)
        (org-assist-publishing-mode 1)
        (org-assist-citation-handling-mode 1)
        (org-assist-working-with-source-code-mode 1))
    (progn
      (org-assist-document-structure-mode 0)
      (org-assist-tables-mode 0)
      (org-assist-hyperlinks-mode 0)
      (org-assist-todo-items-mode 0)
      (org-assist-tags-mode 0)
      (org-assist-properties-columns-mode 0)
      (org-assist-dates-times-mode 0)
      (org-assist-refiling-archiving-mode 0)
      (org-assist-capture-attachments-mode 0)
      (org-assist-agenda-views-mode 0)
      (org-assist-markup-rich-contents-mode 0)
      (org-assist-exporting-mode 0)
      (org-assist-publishing-mode 0)
      (org-assist-citation-handling-mode 0)
      (org-assist-working-with-source-code-mode 0))))

(provide 'org-assist)

;;; org-assist.el ends here
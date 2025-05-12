;;; org-assist-document-structure.el --- Org Assist: Document structure helpers -*- lexical-binding: t; -*-

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
;; This module provides functions to simplify and enhance Org-mode document structure tasks
;; such as headline management, visibility cycling, block insertion, and sparse trees.

;;; Code:

;;; Code:

(require 'org)
(require 'org-element)

(defvar org-assist-document-structure-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "h") #'org-assist-insert-headline)
    (define-key map (kbd "H") #'org-assist-insert-child-headline)
    (define-key map (kbd "<") #'org-assist-promote-headline)
    (define-key map (kbd ">") #'org-assist-demote-headline)
    (define-key map (kbd "v") #'org-assist-toggle-visibility)
    (define-key map (kbd "g") #'org-assist-cycle-global)
    (define-key map (kbd "l") #'org-assist-cycle-local)
    (define-key map (kbd "s") #'org-assist-set-initial-visibility)
    (define-key map (kbd "n") #'org-assist-goto-next-heading)
    (define-key map (kbd "p") #'org-assist-goto-previous-heading)
    (define-key map (kbd "u") #'org-assist-goto-parent-heading)
    (define-key map (kbd "t") #'org-assist-goto-top-level-heading)
    (define-key map (kbd "M-u") #'org-assist-move-subtree-up)
    (define-key map (kbd "M-d") #'org-assist-move-subtree-down)
    (define-key map (kbd "c") #'org-assist-copy-subtree)
    (define-key map (kbd "x") #'org-assist-cut-subtree)
    (define-key map (kbd "y") #'org-assist-insert-subtree-from-clipboard)
    (define-key map (kbd "b") #'org-assist-insert-block)
    (define-key map (kbd "B") #'org-assist-insert-src-block)
    (define-key map (kbd "m") #'org-assist-block-menu)
    (define-key map (kbd "w") #'org-assist-wrap-region-in-block)
    (define-key map (kbd "T") #'org-assist-sparse-todo)
    (define-key map (kbd "#") #'org-assist-sparse-tag)
    (define-key map (kbd "/") #'org-assist-sparse-regexp)
    (define-key map (kbd "?") #'org-assist-sparse-interactive)
    (define-key map (kbd "-") #'org-assist-insert-bullet-list)
    (define-key map (kbd "1") #'org-assist-insert-numbered-list)
    (define-key map (kbd "~") #'org-assist-convert-list-type)
    (define-key map (kbd "[") #'org-assist-promote-list-item)
    (define-key map (kbd "]") #'org-assist-demote-list-item)
    (define-key map (kbd "d") #'org-assist-insert-drawer)
    (define-key map (kbd "D") #'org-assist-insert-properties-drawer)
    (define-key map (kbd "V") #'org-assist-toggle-drawer-visibility)
    map)
  "Keymap for `org-assist-document-structure-mode'.")

;;;###autoload
(define-minor-mode org-assist-document-structure-mode
  "Minor mode for Org document structure helpers."
  :lighter " OrgStruct"
  :keymap org-assist-document-structure-map)

;;; Headlines
(defun org-assist-document-structure-insert-headline ()
  (interactive)
  (org-end-of-subtree t t)
  (insert (format "\n%s " (make-string (org-current-level) ?*))))

(defun org-assist-document-structure-insert-child-headline ()
  (interactive)
  (org-end-of-subtree t t)
  (insert (format "\n%s " (make-string (1+ (org-current-level)) ?*))))

(defun org-assist-document-structure-promote-headline ()
  (interactive)
  (org-promote-subtree))

(defun org-assist-document-structure-demote-headline ()
  (interactive)
  (org-demote-subtree))

(defun org-assist-document-structure-cycle-headline-levels ()
  (interactive)
  (let ((current (org-current-level)))
    (save-excursion
      (org-back-to-heading t)
      (delete-region (point) (progn (skip-chars-forward "*") (point)))
      (insert (make-string (if (>= current 5) 1 (1+ current)) ?*)))))

;;; Visibility
(defun org-assist-document-structure-toggle-visibility () (interactive) (org-cycle))
(defun org-assist-document-structure-cycle-global () (interactive) (org-global-cycle))
(defun org-assist-document-structure-cycle-local () (interactive) (org-cycle))

(defun org-assist-document-structure-set-initial-visibility ()
  (interactive)
  (let ((vis (completing-read "Startup visibility: " '("showall" "overview" "showeverything"))))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^#\+STARTUP:.*" nil t)
          (replace-match (concat "#+STARTUP: " vis))
        (insert "#+STARTUP: " vis "\n")))))

;;; Motion
(defun org-assist-document-structure-goto-next-heading () (interactive) (org-next-visible-heading 1))
(defun org-assist-document-structure-goto-previous-heading () (interactive) (org-previous-visible-heading 1))
(defun org-assist-document-structure-goto-parent-heading () (interactive) (outline-up-heading 1))
(defun org-assist-document-structure-goto-top-level-heading () (interactive) (org-back-to-heading t))

;;; Structure Editing
(defun org-assist-document-structure-move-subtree-up () (interactive) (org-move-subtree-up))
(defun org-assist-document-structure-move-subtree-down () (interactive) (org-move-subtree-down))
(defun org-assist-document-structure-copy-subtree () (interactive) (org-copy-subtree))
(defun org-assist-document-structure-cut-subtree () (interactive) (org-cut-subtree))
(defun org-assist-document-structure-insert-subtree-from-clipboard () (interactive) (org-paste-subtree))

;;; Sparse Trees
(defun org-assist-document-structure-sparse-todo () (interactive) (org-sparse-tree nil "TODO"))
(defun org-assist-document-structure-sparse-tag () (interactive) (org-sparse-tree nil (read-string "Tag: ")))
(defun org-assist-document-structure-sparse-regexp () (interactive) (org-sparse-tree nil (read-regexp "Regexp: ")))
(defun org-assist-document-structure-sparse-interactive ()
  (interactive)
  (let ((choice (completing-read "Sparse tree by: " '("TODO" "TAG" "REGEXP"))))
    (cl-case (intern (downcase choice))
      (todo (org-assist-sparse-todo))
      (tag (call-interactively 'org-assist-sparse-tag))
      (regexp (call-interactively 'org-assist-sparse-regexp)))))

;;; Lists
(defun org-assist-document-structure-insert-bullet-list ()
  (interactive)
  (insert "- "))

(defun org-assist-document-structure-insert-numbered-list ()
  (interactive)
  (insert "1. "))

(defun org-assist-document-structure-convert-list-type ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "[ 	]*[-+] ")
      (replace-match "1. "))
    (when (looking-at "[ 	]*[0-9]+\. ")
      (replace-match "- "))))

(defun org-assist-document-structure-promote-list-item () (interactive) (org-indent-item -1))
(defun org-assist-document-structure-demote-list-item () (interactive) (org-indent-item 1))

;;; Drawers
(defun org-assist-document-structure-insert-drawer (name)
  (interactive "sDrawer name: ")
  (insert (format ":%s:\n\n:%s:\n" (upcase name) (upcase name)))
  (forward-line -1))

(defun org-assist-document-structure-insert-properties-drawer ()
  (interactive)
  (insert ":PROPERTIES:\n:CREATED: \n:END:\n")
  (forward-line -2))

(defun org-assist-document-structure-toggle-drawer-visibility ()
  (interactive)
  (save-excursion
    (outline-next-heading)
    (outline-hide-subtree)))

;;; Blocks
(defun org-assist-document-structure-insert-block (type)
  (interactive (list (completing-read "Block type: " '("quote" "example" "center" "comment" "verse" "src"))))
  (insert (format "#+begin_%s\n\n#+end_%s\n" type type))
  (forward-line -1))

(defun org-assist-document-structure-insert-src-block ()
  (interactive)
  (let ((lang (completing-read "Source language: " '("emacs-lisp" "python" "sh" "org" "html"))))
    (insert (format "#+begin_src %s\n\n#+end_src\n" lang))
    (forward-line -1)))

(defun org-assist-document-structure-block-menu ()
  (interactive)
  (call-interactively #'org-assist-insert-block))

(defun org-assist-document-structure-wrap-region-in-block (type)
  (interactive (list (completing-read "Block type: " '("quote" "example" "center" "comment" "verse" "src"))))
  (let ((beg (region-beginning))
        (end (region-end)))
    (goto-char end)
    (insert (format "\n#+end_%s" type))
    (goto-char beg)
    (insert (format "#+begin_%s\n" type))))

(provide 'org-assist-document-structure)

;;; org-assist-document-structure.el ends here
# Org Assist

**Org Assist** is a modular set of Emacs minor modes designed to simplify and streamline common Org-mode tasks. It provides intuitive helper commands and keybindings for everything from document structure to citations, code blocks, exporting, and more.

## ✨ Features

- 🧱 Document structure editing (headlines, visibility, motion, drawers)
- 📋 Tables, properties, column views
- ✅ TODOs, checkboxes, priorities
- 🏷️ Tags, inheritance, tag-based sparse trees
- 📆 Timestamps, deadlines, scheduling, clocking
- 🧭 Refiling and archiving
- 📎 Capture templates and attachments
- 🗓️ Agenda view launchers
- 🖋️ Rich markup (bold, italics, LaTeX fragments)
- 📤 Exporting to HTML, LaTeX, Markdown, PDF, etc.
- 🌐 Publishing system helpers
- 📚 Citation support with `org-cite`
- 🔢 Source code block insertion, editing, and evaluation

## 🧩 Installation

Clone the repo and load the path in your Emacs config:

```elisp
(add-to-list 'load-path "~/.emacs.d/lisp/org-assist")
(require 'org-assist)
(add-hook 'org-mode-hook #'org-assist-mode)
```

## 🔄 Modularity

Each submodule is also a standalone minor mode and can be activated individually.

## 🧠 Philosophy

Org Assist doesn’t reinvent Org-mode — it enhances it. Its goal is to make Org more accessible, especially for new users, while also improving workflow efficiency for power users.

## 📁 Modules

| Module                        | Purpose                                        |
|------------------------------|------------------------------------------------|
| `document-structure.el`      | Headlines, visibility, motion, sparse trees    |
| `tables.el`                  | Table creation, alignment, formulas            |
| `hyperlinks.el`              | Inserting and navigating links                 |
| `TODO-items.el`              | TODO states, checkboxes, priorities            |
| `tags.el`                    | Tagging, searching, inheritance                |
| `properties-columns.el`      | Properties and column views                    |
| `dates-times.el`             | Scheduling, deadlines, clocking                |
| `refiling-archiving.el`      | Refiling and archiving entries                 |
| `capture-attachments.el`     | Captures and file attachments                  |
| `agenda-views.el`            | Agenda and custom views                        |
| `markup-rich-contents.el`    | Emphasis, LaTeX, symbols                       |
| `exporting.el`               | Export to HTML, LaTeX, Markdown, etc.         |
| `publishing.el`              | Manage publishing projects                     |
| `citation-handling.el`       | Citation styles and bibliography handling      |
| `working-with-source-code.el`| Source block editing and evaluation            |

## 🛠️ License

This project is licensed under the GNU Public License v3. See `LICENSE` for details.

## 👤 Author

**danoricm** – [github.com/danoricm](https://github.com/danoricm)

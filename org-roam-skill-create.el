;;; org-roam-skill-create.el --- Note creation functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Tahir Butt
;; Keywords: outlines convenience

;;; Commentary:
;; Functions for creating org-roam notes programmatically.

;;; Code:

(require 'org-roam)
(require 'org-id)
(require 'org-roam-skill-core)

;;;###autoload
(defun org-roam-skill-create-note (title &optional tags content no-format)
  "Create a new org-roam note with TITLE, optional TAGS and CONTENT.
Automatically detect filename format and head content from capture templates.
Work with any org-roam configuration - no customization required.

CONTENT is automatically formatted to org-mode syntax using pandoc unless:
- NO-FORMAT is non-nil, or
- CONTENT starts with 'NO_FORMAT:' prefix

This handles both markdown and org-mode input, normalizing to clean org format.

Return the file path of the created note."
  (let* ((file-name (org-roam-skill--expand-filename title))
         (file-path (expand-file-name file-name org-roam-directory))
         (node-id (org-id-uuid))
         (head-content (org-roam-skill--get-head-content))
         ;; Format content using pandoc (handles markdown/org input)
         (formatted-content (when content
                              (org-roam-skill--format-content content no-format))))

    ;; Create the file with proper org-roam structure
    (with-temp-file file-path
      ;; Insert PROPERTIES block with ID
      (insert ":PROPERTIES:\n")
      (insert (format ":ID:       %s\n" node-id))
      (insert ":END:\n")

      ;; Insert head content if template specifies it
      (when (and head-content (not (string-empty-p head-content)))
        (let* ((expanded-head
                ;; First expand ${title}
                (replace-regexp-in-string "\\${title}" title head-content))
               ;; Then expand time format specifiers
               (expanded-head (org-roam-skill--expand-time-formats expanded-head)))
          (insert expanded-head)
          (unless (string-suffix-p "\n" expanded-head)
            (insert "\n"))))

      ;; If head content doesn't include title, add it
      (unless (string-match-p "#\\+\\(?:title\\|TITLE\\):" (or head-content ""))
        (insert (format "#+TITLE: %s\n" title)))

      ;; Insert filetags if provided (sanitize to remove hyphens)
      (when tags
        (let ((sanitized-tags
               (mapcar #'org-roam-skill--sanitize-tag tags)))
          (insert (format "#+FILETAGS: :%s:\n"
                          (mapconcat (lambda (tag) tag) sanitized-tags ":")))))

      ;; Add blank line after frontmatter
      (insert "\n")

      ;; Insert formatted content if provided
      (when formatted-content
        (insert formatted-content)
        (unless (string-suffix-p "\n" formatted-content)
          (insert "\n"))))

    ;; Sync database to register the new note
    (org-roam-db-sync)

    ;; Return the file path
    file-path))

;;;###autoload
(defun org-roam-skill-create-note-with-content (title content &optional tags)
  "Create a new org-roam note with TITLE, CONTENT and optional TAGS.
This is an alias for org-roam-skill-create-note with different arg order.
Return the file path of the created note."
  (org-roam-skill-create-note title tags content))

(provide 'org-roam-skill-create)
;;; org-roam-skill-create.el ends here

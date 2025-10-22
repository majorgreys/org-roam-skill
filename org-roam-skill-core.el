;;; org-roam-skill-core.el --- Core utilities for org-roam-skill -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Tahir Butt
;; Keywords: outlines convenience

;;; Commentary:
;; Core utility functions shared across org-roam-skill modules.

;;; Code:

(require 'org-roam)

(defun org-roam-skill--sanitize-tag (tag)
  "Sanitize TAG by replacing hyphens with underscores.
Org tags cannot contain hyphens."
  (replace-regexp-in-string "-" "_" tag))

(defun org-roam-skill--with-node-context (title-or-id function)
  "Execute FUNCTION with point at the node identified by TITLE-OR-ID.
FUNCTION receives the node as an argument.
Returns the result of FUNCTION."
  (let* ((node (if (and (stringp title-or-id)
                        (string-match-p "^[0-9a-f]\\{8\\}-" title-or-id))
                   (org-roam-node-from-id title-or-id)
                 (org-roam-node-from-title-or-alias title-or-id)))
         (file (when node (org-roam-node-file node)))
         (node-id (when node (org-roam-node-id node))))
    (unless node
      (error "Node not found: %s" title-or-id))
    (unless (file-exists-p file)
      (error "File not found: %s" file))

    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        ;; Search for the node's ID property
        (if (re-search-forward
             (format ":ID:[ \t]+%s" (regexp-quote node-id)) nil t)
            (progn
              ;; Move to the beginning of the entry
              (org-back-to-heading-or-point-min t)
              (funcall function node))
          (error "Could not locate node in file: %s" title-or-id))))))

(defun org-roam-skill--get-filename-format ()
  "Extract filename format from user's org-roam capture templates.
Return the filename pattern from the default template, or a fallback."
  (let* ((default-template (assoc "d" org-roam-capture-templates))
         ;; Skip key, description, type, template-content to get to plist
         (plist (cdr (cdr (cdr (cdr default-template)))))
         (target (plist-get plist :target)))
    (if (and target (eq (car target) 'file+head))
        ;; Extract first argument of file+head
        (nth 1 target)
      ;; Fallback to timestamp-only if no template found
      "%<%Y%m%d%H%M%S>.org")))

(defun org-roam-skill--expand-filename (title)
  "Generate a filename for TITLE using the user's configured format.
Expand placeholders like %<...>, ${slug}, ${title}, etc."
  (let* ((format-string (org-roam-skill--get-filename-format))
         ;; Create slug manually: lowercase, replace spaces with underscores
         (slug (replace-regexp-in-string " " "_" (downcase title)))
         (timestamp (format-time-string "%Y%m%d%H%M%S"))
         (filename format-string))

    ;; Replace common placeholders
    ;; Handle %<...> time format
    (when (string-match "%<\\([^>]+\\)>" filename)
      (let ((time-format (match-string 1 filename)))
        (setq filename (replace-regexp-in-string
                       "%<[^>]+>"
                       (format-time-string time-format)
                       filename))))

    ;; Replace ${slug}
    (setq filename (replace-regexp-in-string "\\${slug}" slug filename))

    ;; Replace ${title}
    (setq filename (replace-regexp-in-string "\\${title}" title filename))

    ;; Ensure .org extension
    (unless (string-suffix-p ".org" filename)
      (setq filename (concat filename ".org")))

    filename))

(defun org-roam-skill--get-head-content ()
  "Extract head content from user's org-roam capture template.
Return the head template string, or nil if not found."
  (let* ((default-template (assoc "d" org-roam-capture-templates))
         (plist (cdr (cdr (cdr (cdr default-template)))))
         (target (plist-get plist :target)))
    (when (and target (eq (car target) 'file+head))
      ;; Second argument of file+head is the head content
      (nth 2 target))))

(provide 'org-roam-skill-core)
;;; org-roam-skill-core.el ends here

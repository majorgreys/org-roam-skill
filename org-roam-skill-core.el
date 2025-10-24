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

(defun org-roam-skill--expand-time-formats (template-string)
  "Expand time format specifiers in TEMPLATE-STRING.
Handles:
- %<format> - custom time format (e.g., %<%Y-%m-%d>)
- %U - inactive timestamp with time [2025-10-23 Thu 15:53]
- %u - inactive timestamp without time [2025-10-23 Thu]
- %T - active timestamp with time <2025-10-23 Thu 15:53>
- %t - active timestamp without time <2025-10-23 Thu>

Returns the expanded string with all time formats replaced."
  (let ((result template-string)
        (case-fold-search nil))  ; Make regex matching case-sensitive
    ;; Expand %<...> custom time formats
    (while (string-match "%<\\([^>]+\\)>" result)
      (let ((time-format (match-string 1 result)))
        (setq result (replace-match
                     (format-time-string time-format)
                     t t result))))

    ;; Expand %U - inactive timestamp with time (order matters: do %U before %u)
    (setq result (replace-regexp-in-string
                 "%U"
                 (format-time-string "[%Y-%m-%d %a %H:%M]")
                 result t t))

    ;; Expand %u - inactive timestamp without time
    (setq result (replace-regexp-in-string
                 "%u"
                 (format-time-string "[%Y-%m-%d %a]")
                 result t t))

    ;; Expand %T - active timestamp with time (order matters: do %T before %t)
    (setq result (replace-regexp-in-string
                 "%T"
                 (format-time-string "<%Y-%m-%d %a %H:%M>")
                 result t t))

    ;; Expand %t - active timestamp without time
    (setq result (replace-regexp-in-string
                 "%t"
                 (format-time-string "<%Y-%m-%d %a>")
                 result t t))

    result))

(defun org-roam-skill--detect-format (content)
  "Detect if CONTENT is org-mode or markdown format.
Returns 'org if org-mode syntax detected, 'markdown otherwise.
Detection heuristics:
- Org headings: lines starting with * followed by space
- Org emphasis: /italic/ *bold* _underline_
- Org blocks: #+begin_ #+end_
- Otherwise assume markdown (safer default for mixed content)."
  (cond
   ;; Check for org headings at start of line
   ((string-match-p "^\\* " content) 'org)
   ;; Check for org blocks
   ((string-match-p "^[ \t]*#\\+\\(begin\\|end\\)_" content) 'org)
   ;; Check for org-style properties drawer
   ((string-match-p "^[ \t]*:PROPERTIES:" content) 'org)
   ;; Check for org-style emphasis with /italic/
   ((string-match-p "/[^/\n]+/" content) 'org)
   ;; Default to markdown (handles plain text well)
   (t 'markdown)))

(defun org-roam-skill--format-content (content &optional no-format)
  "Format CONTENT to org-mode syntax using pandoc.
Handles both markdown and org-mode input, normalizing to clean org format.
If NO-FORMAT is non-nil or content starts with 'NO_FORMAT:', skip formatting.
Returns formatted content or original if formatting fails/disabled."
  (cond
   ;; Skip formatting if explicitly disabled
   ((or no-format
        (and (stringp content)
             (string-prefix-p "NO_FORMAT:" content)))
    (if (string-prefix-p "NO_FORMAT:" content)
        (substring content 10)  ; Strip the NO_FORMAT: prefix
      content))

   ;; Skip formatting if content is empty
   ((or (not content) (string-empty-p content))
    content)

   ;; Format using pandoc
   (t
    (condition-case err
        (with-temp-buffer
          (insert content)
          (let* ((detected-format (org-roam-skill--detect-format content))
                 (input-format (symbol-name detected-format))
                 (output-buffer (generate-new-buffer " *pandoc-output*"))
                 (exit-code (call-process-region
                            (point-min) (point-max)
                            "pandoc"
                            nil output-buffer nil
                            "-f" input-format
                            "-t" "org"
                            "--wrap=none")))
            (with-current-buffer output-buffer
              (let ((result (buffer-string)))
                (kill-buffer output-buffer)
                ;; Check if pandoc succeeded
                (if (and (= exit-code 0) (not (string-empty-p result)))
                    ;; Remove CUSTOM_ID properties that pandoc adds
                    (replace-regexp-in-string
                     ":PROPERTIES:\n:CUSTOM_ID:.*\n:END:\n" ""
                     result)
                  ;; Return original content if pandoc failed
                  content)))))
      ;; If pandoc fails, return original content
      (error content)))))

(provide 'org-roam-skill-core)
;;; org-roam-skill-core.el ends here

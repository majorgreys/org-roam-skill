;;; create-note.el --- Create org-roam notes programmatically

(require 'org-roam)
(require 'org-id)

(defun org-roam-skill--get-filename-format ()
  "Extract filename format from user's org-roam capture templates.
Returns the filename pattern from the default template, or a fallback."
  (let* ((default-template (assoc "d" org-roam-capture-templates))
         (template-str (when default-template (format "%S" default-template))))
    (cond
     ;; Extract pattern from template string
     ((and template-str (string-match "file+head \"\\([^\"]+\\)\"" template-str))
      (match-string 1 template-str))
     ;; Fallback to timestamp-only if no template found
     (t "%<%Y%m%d%H%M%S>.org"))))

(defun org-roam-skill--expand-filename (title)
  "Generate a filename for TITLE using the user's configured format.
Expands placeholders like %<...>, ${slug}, ${title}, etc."
  (let* ((format-string (org-roam-skill--get-filename-format))
         (slug (org-roam-node-slug (org-roam-node-create :title title)))
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
Returns the head content string, or empty string if not found."
  (let* ((default-template (assoc "d" org-roam-capture-templates))
         (template-str (when default-template (format "%S" default-template))))
    (cond
     ;; Extract head content from template string
     ((and template-str (string-match "file+head \"[^\"]+\" \"\\([^\"]*\\)\"" template-str))
      (let ((head (match-string 1 template-str)))
        ;; Unescape the string
        (replace-regexp-in-string "\\\\n" "\n" head)))
     ;; Fallback to empty (org-roam adds title automatically)
     (t ""))))

(defun create-org-roam-note (title &optional tags content)
  "Create a new org-roam note with TITLE, optional TAGS and CONTENT.
Automatically detects filename format and head content from user's capture templates.
Works with any org-roam configuration - no customization required.
Returns the file path of the created note."
  (let* ((file-name (org-roam-skill--expand-filename title))
         (file-path (expand-file-name file-name org-roam-directory))
         (node-id (org-id-uuid))
         (head-content (org-roam-skill--get-head-content)))

    ;; Create the file with proper org-roam structure
    (with-temp-file file-path
      ;; Insert PROPERTIES block with ID
      (insert ":PROPERTIES:\n")
      (insert (format ":ID:       %s\n" node-id))
      (insert ":END:\n")

      ;; Insert head content if template specifies it
      (when (and head-content (not (string-empty-p head-content)))
        (let ((expanded-head (replace-regexp-in-string "\\${title}" title head-content)))
          (insert expanded-head)
          (unless (string-suffix-p "\n" expanded-head)
            (insert "\n"))))

      ;; If head content doesn't include title, add it
      (unless (string-match-p "#\\+title:" (or head-content ""))
        (insert (format "#+title: %s\n" title)))

      ;; Insert filetags if provided
      (when tags
        (insert "#+filetags: "
                (mapconcat (lambda (tag) (format ":%s:" tag)) tags " ")
                "\n"))

      ;; Add blank line after frontmatter
      (insert "\n")

      ;; Insert content if provided
      (when content
        (insert content "\n")))

    ;; Sync database to register the new note
    (org-roam-db-sync)

    ;; Return the file path
    file-path))

(defun create-org-roam-note-with-content (title content &optional tags)
  "Create a new org-roam note with TITLE, CONTENT and optional TAGS.
This is now just an alias for create-org-roam-note.
Returns the file path of the created note."
  (create-org-roam-note title tags content))

(provide 'create-note)
;;; create-note.el ends here

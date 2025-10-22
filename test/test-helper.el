;;; test-helper.el --- Test utilities for org-roam-skill -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared test fixtures and utilities using Buttercup

;;; Code:

(require 'buttercup)
(require 'org-roam)
(require 'org-roam-skill)

(defvar org-roam-skill-test-directory nil
  "Temporary directory for test database.")

(defun org-roam-skill-test--setup ()
  "Set up temporary org-roam directory for testing."
  (setq org-roam-skill-test-directory (make-temp-file "org-roam-test-" t))
  (setq org-roam-directory org-roam-skill-test-directory
        org-roam-db-location
        (expand-file-name "org-roam.db" org-roam-skill-test-directory))
  ;; Initialize database
  (org-roam-db-sync))

(defun org-roam-skill-test--teardown ()
  "Clean up temporary org-roam directory."
  (when (and org-roam-skill-test-directory
             (file-exists-p org-roam-skill-test-directory))
    ;; Close database connection
    (when (fboundp 'org-roam-db--close)
      (org-roam-db--close))
    ;; Delete temp directory
    (delete-directory org-roam-skill-test-directory t)
    (setq org-roam-skill-test-directory nil)))

(defun org-roam-skill-test--create-test-note (title tags &optional content)
  "Create a test note with TITLE, TAGS, and optional CONTENT.
Returns the file path."
  (create-org-roam-note title tags content))

(defun org-roam-skill-test--count-nodes ()
  "Return the number of nodes in the test database."
  (length (org-roam-node-list)))

(defun org-roam-skill-test--get-note-content (file-path)
  "Get the content of the note at FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun org-roam-skill-test--node-exists-p (title)
  "Check if a node with TITLE exists."
  (not (null (org-roam-node-from-title-or-alias title))))

(provide 'test-helper)
;;; test-helper.el ends here

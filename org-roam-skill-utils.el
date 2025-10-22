;;; org-roam-skill-utils.el --- Utility functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Tahir Butt
;; Keywords: outlines convenience

;;; Commentary:
;; Utility functions for org-roam-skill including orphan detection and stats.

;;; Code:

(require 'org-roam)
(require 'seq)

;;;###autoload
(defun org-roam-skill-check-setup ()
  "Check if org-roam is properly set up.
Return a plist with status information."
  (list :org-roam-loaded (featurep 'org-roam)
        :directory org-roam-directory
        :directory-exists (file-exists-p org-roam-directory)
        :database-location org-roam-db-location
        :database-exists (file-exists-p org-roam-db-location)
        :node-count (length (org-roam-node-list))))

;;;###autoload
(defun org-roam-skill-get-note-info (title)
  "Get comprehensive information about a note by TITLE.
Return a formatted string with all note details."
  (let ((node (org-roam-node-from-title-or-alias title)))
    (if node
        (format
         "Title: %s\nID: %s\nFile: %s\nTags: %s\nAliases: %s\nRefs: %s\nBacklinks: %d\nLevel: %d"
         (org-roam-node-title node)
         (org-roam-node-id node)
         (org-roam-node-file node)
         (or (org-roam-node-tags node) "none")
         (or (org-roam-node-aliases node) "none")
         (or (org-roam-node-refs node) "none")
         (length (org-roam-backlinks-get node))
         (org-roam-node-level node))
      "Note not found")))

;;;###autoload
(defun org-roam-skill-list-recent-notes (n)
  "List the N most recently modified notes.
Return a list of (id title file mtime) tuples."
  (mapcar
   (lambda (node)
     (list (org-roam-node-id node)
           (org-roam-node-title node)
           (org-roam-node-file node)
           (org-roam-node-file-mtime node)))
   (seq-take
    (seq-sort
     (lambda (a b)
       (time-less-p (org-roam-node-file-mtime b)
                    (org-roam-node-file-mtime a)))
     (org-roam-node-list))
    n)))

;;;###autoload
(defun org-roam-skill-find-orphan-notes ()
  "Find notes that have no backlinks and no forward links.
Return a list of (id title file) tuples for orphaned notes."
  (seq-filter
   (lambda (node-info)
     (let* ((node (org-roam-node-from-id (car node-info)))
            (backlinks (org-roam-backlinks-get node))
            (file (org-roam-node-file node))
            (has-forward-links nil))
       ;; Check for forward links
       (when (and file (file-exists-p file))
         (with-temp-buffer
           (insert-file-contents file)
           (goto-char (point-min))
           (when (re-search-forward "\\[\\[id:" nil t)
             (setq has-forward-links t))))
       ;; Return if both are empty
       (and (null backlinks) (not has-forward-links))))
   (mapcar
    (lambda (node)
      (list (org-roam-node-id node)
            (org-roam-node-title node)
            (org-roam-node-file node)))
    (org-roam-node-list))))

;;;###autoload
(defun org-roam-skill-get-graph-stats ()
  "Get statistics about the org-roam graph.
Return a plist with various statistics."
  (let* ((nodes (org-roam-node-list))
         (total-nodes (length nodes))
         (total-links 0)
         ;; Forward declare to avoid dependency on org-roam-skill-tags
         (tags (sort
                (delete-dups
                 (flatten-list
                  (mapcar #'org-roam-node-tags nodes)))
                #'string<)))
    (dolist (node nodes)
      (setq total-links (+ total-links (length (org-roam-backlinks-get node)))))
    (list :total-notes total-nodes
          :total-links total-links
          :unique-tags (length tags)
          :average-links-per-note (if (> total-nodes 0)
                                      (/ (float total-links) total-nodes)
                                    0))))

(defun org-roam-skill--format-buffer ()
  "Format the current org buffer, aligning tables and cleaning up structure.
Should be called with point in an org-mode buffer."
  (save-excursion
    ;; Align all tables in the buffer
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*|" nil t)
      (beginning-of-line)
      (when (org-at-table-p)
        (org-table-align))
      (forward-line 1))

    ;; Clean up spacing around headings and content
    (org-mode-restart)))

;;;###autoload
(defun format-org-roam-note (title-or-id)
  "Format the org-roam note identified by TITLE-OR-ID.
Aligns tables and cleans up structure. Returns t on success."
  (let* ((node (if (and (stringp title-or-id)
                        (string-match-p "^[0-9a-f]\\{8\\}-" title-or-id))
                   (org-roam-node-from-id title-or-id)
                 (org-roam-node-from-title-or-alias title-or-id)))
         (file (when node (org-roam-node-file node))))
    (unless node
      (error "Node not found: %s" title-or-id))
    (unless (file-exists-p file)
      (error "File not found: %s" file))

    (with-current-buffer (find-file-noselect file)
      (org-roam-skill--format-buffer)
      (save-buffer)
      t)))

(provide 'org-roam-skill-utils)
;;; org-roam-skill-utils.el ends here

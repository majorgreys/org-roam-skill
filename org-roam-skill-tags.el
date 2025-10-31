;;; org-roam-skill-tags.el --- Tag management functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Tahir Butt
;; Keywords: outlines convenience

;;; Commentary:
;; Functions for managing tags in org-roam notes.

;;; Code:

(require 'org-roam)
(require 'org-roam-skill-core)
(require 'seq)

;;;###autoload
(defun org-roam-skill-list-all-tags ()
  "Get a list of all unique tags used in org-roam.
Return a sorted list of tag strings."
  (sort
   (delete-dups
    (flatten-list
     (mapcar #'org-roam-node-tags (org-roam-node-list))))
   #'string<))

;;;###autoload
(defun org-roam-skill-count-notes-by-tag ()
  "Count how many notes use each tag.
Return an alist of (tag . count) pairs sorted by count descending."
  (let ((tag-counts (make-hash-table :test 'equal)))
    ;; Count occurrences
    (dolist (node (org-roam-node-list))
      (dolist (tag (org-roam-node-tags node))
        (puthash tag (1+ (gethash tag tag-counts 0)) tag-counts)))
    ;; Convert to sorted alist
    (sort
     (let (result)
       (maphash (lambda (tag count)
                  (push (cons tag count) result))
                tag-counts)
       result)
     (lambda (a b) (> (cdr a) (cdr b))))))

;;;###autoload
(defun org-roam-skill-get-notes-without-tags ()
  "Get all notes that have no tags.
Return a list of (id title file) tuples."
  (mapcar
   (lambda (node)
     (list (org-roam-node-id node)
           (org-roam-node-title node)
           (org-roam-node-file node)))
   (seq-filter
    (lambda (node)
      (null (org-roam-node-tags node)))
    (org-roam-node-list))))

;;;###autoload
(defun org-roam-skill-add-tag (title tag)
  "Add TAG to the note with TITLE.
Sanitize TAG by replacing hyphens with underscores.
Return t if successful, nil otherwise."
  (let* ((node (org-roam-node-from-title-or-alias title))
         (file (when node (org-roam-node-file node)))
         (sanitized-tag (org-roam-skill--sanitize-tag tag)))
    (when (and file (file-exists-p file))
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (if (re-search-forward "^#\\+\\(?:filetags\\|FILETAGS\\):" nil t)
              ;; Tags line exists, append tag
              (progn
                (end-of-line)
                (unless (looking-back (concat ":" sanitized-tag ":") nil)
                  (insert ":" sanitized-tag ":")))
            ;; No tags line, create it
            (when (re-search-forward "^#\\+\\(?:title\\|TITLE\\):" nil t)
              (forward-line 1)
              (insert "#+FILETAGS: :" sanitized-tag ":\n")))
          (save-buffer)
          (org-roam-db-sync)
          t)))))

;;;###autoload
(defun org-roam-skill-remove-tag (title tag)
  "Remove TAG from the note with TITLE.
Sanitize TAG by replacing hyphens with underscores.
Return t if successful, nil otherwise."
  (let* ((node (org-roam-node-from-title-or-alias title))
         (file (when node (org-roam-node-file node)))
         (sanitized-tag (org-roam-skill--sanitize-tag tag)))
    (when (and file (file-exists-p file))
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^#\\+\\(?:filetags\\|FILETAGS\\):" nil t)
            (let ((line-start (line-beginning-position))
                  (line-end (line-end-position)))
              (goto-char line-start)
              (while (re-search-forward
                      (concat ":" sanitized-tag ":") line-end t)
                (replace-match ":" nil nil))
              (save-buffer)
              (org-roam-db-sync)
              t)))))))

(provide 'org-roam-skill-tags)
;;; org-roam-skill-tags.el ends here

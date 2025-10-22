;;; org-roam-skill-search.el --- Search functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Tahir Butt
;; Keywords: outlines convenience

;;; Commentary:
;; Functions for searching org-roam notes by title, tag, and content.

;;; Code:

(require 'org-roam)
(require 'seq)

;;;###autoload
(defun org-roam-skill-search-by-title (search-term)
  "Search org-roam notes by SEARCH-TERM in title.
Return a list of (id title file) tuples."
  (mapcar
   (lambda (node)
     (list (org-roam-node-id node)
           (org-roam-node-title node)
           (org-roam-node-file node)))
   (seq-filter
    (lambda (node)
      (string-match-p (regexp-quote search-term)
                      (org-roam-node-title node)))
    (org-roam-node-list))))

;;;###autoload
(defun org-roam-skill-search-by-tag (tag)
  "Search org-roam notes by TAG.
Return a list of (id title file tags) tuples."
  (mapcar
   (lambda (node)
     (list (org-roam-node-id node)
           (org-roam-node-title node)
           (org-roam-node-file node)
           (org-roam-node-tags node)))
   (seq-filter
    (lambda (node)
      (member tag (org-roam-node-tags node)))
    (org-roam-node-list))))

;;;###autoload
(defun org-roam-skill-search-by-content (search-term)
  "Search org-roam notes by SEARCH-TERM in file content.
Return a list of (id title file) tuples for notes containing the term."
  (let ((results '()))
    (dolist (node (org-roam-node-list))
      (let ((file (org-roam-node-file node)))
        (when (and file (file-exists-p file))
          (with-temp-buffer
            (insert-file-contents file)
            (when (search-forward search-term nil t)
              (push (list (org-roam-node-id node)
                          (org-roam-node-title node)
                          file)
                    results))))))
    (nreverse results)))

;;;###autoload
(defun org-roam-skill-get-node-by-title (title)
  "Get org-roam node by exact TITLE or alias.
Return node details as a plist."
  (let ((node (org-roam-node-from-title-or-alias title)))
    (when node
      (list :id (org-roam-node-id node)
            :title (org-roam-node-title node)
            :file (org-roam-node-file node)
            :tags (org-roam-node-tags node)
            :aliases (org-roam-node-aliases node)
            :refs (org-roam-node-refs node)))))

(provide 'org-roam-skill-search)
;;; org-roam-skill-search.el ends here

;;; org-roam-skill-links.el --- Link management functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Tahir Butt
;; Keywords: outlines convenience

;;; Commentary:
;; Functions for managing backlinks, forward links, and link insertion.

;;; Code:

(require 'org-roam)
(require 'org)

;;;###autoload
(defun org-roam-skill-get-backlinks-by-title (title)
  "Get backlinks for a note with TITLE.
Return a list of (id title file) tuples for notes linking to this note."
  (let ((node (org-roam-node-from-title-or-alias title)))
    (when node
      (mapcar
       (lambda (backlink)
         (let ((source-node (org-roam-backlink-source-node backlink)))
           (list (org-roam-node-id source-node)
                 (org-roam-node-title source-node)
                 (org-roam-node-file source-node))))
       (org-roam-backlinks-get node)))))

;;;###autoload
(defun org-roam-skill-get-backlinks-by-id (node-id)
  "Get backlinks for a note with NODE-ID.
Return a list of (id title file) tuples for notes linking to this note."
  (let ((node (org-roam-node-from-id node-id)))
    (when node
      (mapcar
       (lambda (backlink)
         (let ((source-node (org-roam-backlink-source-node backlink)))
           (list (org-roam-node-id source-node)
                 (org-roam-node-title source-node)
                 (org-roam-node-file source-node))))
       (org-roam-backlinks-get node)))))

;;;###autoload
(defun org-roam-skill-get-forward-links-by-title (title)
  "Get forward links (outgoing links) for a note with TITLE.
Return a list of (id title file) tuples for notes this note links to."
  (let* ((node (org-roam-node-from-title-or-alias title))
         (file (when node (org-roam-node-file node)))
         (links '()))
    (when (and file (file-exists-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward org-link-bracket-re nil t)
          (let* ((link (match-string 1))
                 (target-node (when (string-prefix-p "id:" link)
                                (org-roam-node-from-id (substring link 3)))))
            (when target-node
              (push (list (org-roam-node-id target-node)
                          (org-roam-node-title target-node)
                          (org-roam-node-file target-node))
                    links))))))
    (nreverse links)))

;;;###autoload
(defun org-roam-skill-get-all-connections-by-title (title)
  "Get all connections (backlinks and forward links) for a note with TITLE.
Return a plist with :backlinks and :forward-links."
  (list :backlinks (org-roam-skill-get-backlinks-by-title title)
        :forward-links (org-roam-skill-get-forward-links-by-title title)))

;;;###autoload
(defun org-roam-skill-insert-link (source-file target-title)
  "Insert a link to TARGET-TITLE note in SOURCE-FILE at the end.
Return the inserted link text."
  (let ((target-node (org-roam-node-from-title-or-alias target-title)))
    (when target-node
      (with-current-buffer (find-file-noselect source-file)
        (goto-char (point-max))
        (let ((link-text (org-link-make-string
                          (concat "id:" (org-roam-node-id target-node))
                          (org-roam-node-title target-node))))
          (insert "\n" link-text "\n")
          (save-buffer)
          link-text)))))

;;;###autoload
(defun org-roam-skill-insert-link-in-note (source-title target-title)
  "Insert a link to TARGET-TITLE in the note titled SOURCE-TITLE.
Return the inserted link text."
  (let ((source-node (org-roam-node-from-title-or-alias source-title)))
    (when source-node
      (org-roam-skill-insert-link
       (org-roam-node-file source-node) target-title))))

;;;###autoload
(defun org-roam-skill-create-bidirectional-link (title-a title-b)
  "Create bidirectional links between notes TITLE-A and TITLE-B.
Insert links in both directions."
  (let ((link-a-to-b (org-roam-skill-insert-link-in-note title-a title-b))
        (link-b-to-a (org-roam-skill-insert-link-in-note title-b title-a)))
    (list :a-to-b link-a-to-b :b-to-a link-b-to-a)))

;;;###autoload
(defun org-roam-skill-insert-multiple-links (source-title target-titles)
  "Insert links to multiple TARGET-TITLES in the note titled SOURCE-TITLE.
Return a list of inserted link texts."
  (mapcar
   (lambda (target-title)
     (org-roam-skill-insert-link-in-note source-title target-title))
   target-titles))

(provide 'org-roam-skill-links)
;;; org-roam-skill-links.el ends here

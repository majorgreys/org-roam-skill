;;; search-notes.el --- Search org-roam notes

(require 'org-roam)

(defun search-notes-by-title (search-term)
  "Search org-roam notes by SEARCH-TERM in title.
Returns a list of (id title file) tuples."
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

(defun search-notes-by-tag (tag)
  "Search org-roam notes by TAG.
Returns a list of (id title file tags) tuples."
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

(defun search-notes-by-content (search-term)
  "Search org-roam notes by SEARCH-TERM in file content.
Returns a list of (id title file) tuples for notes containing the term."
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

(defun get-node-by-title (title)
  "Get org-roam node by exact TITLE or alias.
Returns node details as a plist."
  (let ((node (org-roam-node-from-title-or-alias title)))
    (when node
      (list :id (org-roam-node-id node)
            :title (org-roam-node-title node)
            :file (org-roam-node-file node)
            :tags (org-roam-node-tags node)
            :aliases (org-roam-node-aliases node)
            :refs (org-roam-node-refs node)))))

(provide 'search-notes)
;;; search-notes.el ends here

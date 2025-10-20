;;; get-backlinks.el --- Get backlinks for org-roam notes

(require 'org-roam)

(defun get-backlinks-by-title (title)
  "Get backlinks for a note with TITLE.
Returns a list of (id title file) tuples for notes linking to this note."
  (let ((node (org-roam-node-from-title-or-alias title)))
    (when node
      (mapcar
       (lambda (backlink)
         (let ((source-node (org-roam-backlink-source-node backlink)))
           (list (org-roam-node-id source-node)
                 (org-roam-node-title source-node)
                 (org-roam-node-file source-node))))
       (org-roam-backlinks-get node)))))

(defun get-backlinks-by-id (node-id)
  "Get backlinks for a note with NODE-ID.
Returns a list of (id title file) tuples for notes linking to this note."
  (let ((node (org-roam-node-from-id node-id)))
    (when node
      (mapcar
       (lambda (backlink)
         (let ((source-node (org-roam-backlink-source-node backlink)))
           (list (org-roam-node-id source-node)
                 (org-roam-node-title source-node)
                 (org-roam-node-file source-node))))
       (org-roam-backlinks-get node)))))

(defun get-forward-links-by-title (title)
  "Get forward links (outgoing links) for a note with TITLE.
Returns a list of (id title file) tuples for notes this note links to."
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

(defun get-all-connections-by-title (title)
  "Get all connections (backlinks and forward links) for a note with TITLE.
Returns a plist with :backlinks and :forward-links."
  (list :backlinks (get-backlinks-by-title title)
        :forward-links (get-forward-links-by-title title)))

(provide 'get-backlinks)
;;; get-backlinks.el ends here

;;; list-tags.el --- List and manage tags in org-roam

(require 'org-roam)

(defun list-all-tags ()
  "Get a list of all unique tags used in org-roam.
Returns a sorted list of tag strings."
  (sort
   (delete-dups
    (flatten-list
     (mapcar #'org-roam-node-tags (org-roam-node-list))))
   #'string<))

(defun count-notes-by-tag ()
  "Count how many notes use each tag.
Returns an alist of (tag . count) pairs sorted by count descending."
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

(defun get-notes-without-tags ()
  "Get all notes that have no tags.
Returns a list of (id title file) tuples."
  (mapcar
   (lambda (node)
     (list (org-roam-node-id node)
           (org-roam-node-title node)
           (org-roam-node-file node)))
   (seq-filter
    (lambda (node)
      (null (org-roam-node-tags node)))
    (org-roam-node-list))))

(defun add-tag-to-note (title tag)
  "Add TAG to the note with TITLE.
Returns t if successful, nil otherwise."
  (let* ((node (org-roam-node-from-title-or-alias title))
         (file (when node (org-roam-node-file node))))
    (when (and file (file-exists-p file))
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (if (re-search-forward "^#\\+filetags:" nil t)
              ;; Tags line exists, append tag
              (progn
                (end-of-line)
                (unless (looking-back (concat ":" tag ":") nil)
                  (insert ":" tag ":")))
            ;; No tags line, create it
            (when (re-search-forward "^#\\+title:" nil t)
              (forward-line 1)
              (insert "#+filetags: :" tag ":\n")))
          (save-buffer)
          (org-roam-db-sync)
          t)))))

(defun remove-tag-from-note (title tag)
  "Remove TAG from the note with TITLE.
Returns t if successful, nil otherwise."
  (let* ((node (org-roam-node-from-title-or-alias title))
         (file (when node (org-roam-node-file node))))
    (when (and file (file-exists-p file))
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^#\\+filetags:" nil t)
            (let ((line-start (line-beginning-position))
                  (line-end (line-end-position)))
              (goto-char line-start)
              (while (re-search-forward (concat ":" tag ":") line-end t)
                (replace-match ":" nil nil))
              (save-buffer)
              (org-roam-db-sync)
              t)))))))

(provide 'list-tags)
;;; list-tags.el ends here

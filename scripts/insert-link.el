;;; insert-link.el --- Insert links in org-roam notes

(require 'org-roam)

(defun insert-link-to-note (source-file target-title)
  "Insert a link to TARGET-TITLE note in SOURCE-FILE at the end.
Returns the inserted link text."
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

(defun insert-link-in-note-by-title (source-title target-title)
  "Insert a link to TARGET-TITLE in the note titled SOURCE-TITLE.
Returns the inserted link text."
  (let ((source-node (org-roam-node-from-title-or-alias source-title)))
    (when source-node
      (insert-link-to-note (org-roam-node-file source-node) target-title))))

(defun create-bidirectional-link (title-a title-b)
  "Create bidirectional links between notes TITLE-A and TITLE-B.
Inserts links in both directions."
  (let ((link-a-to-b (insert-link-in-note-by-title title-a title-b))
        (link-b-to-a (insert-link-in-note-by-title title-b title-a)))
    (list :a-to-b link-a-to-b :b-to-a link-b-to-a)))

(defun insert-multiple-links (source-title target-titles)
  "Insert links to multiple TARGET-TITLES in the note titled SOURCE-TITLE.
Returns a list of inserted link texts."
  (mapcar
   (lambda (target-title)
     (insert-link-in-note-by-title source-title target-title))
   target-titles))

(provide 'insert-link)
;;; insert-link.el ends here

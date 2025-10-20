;;; attach-file.el --- Attach files to org-roam notes using org-attach

(require 'org-roam)
(require 'org-attach)

(defun org-roam-skill--with-node-context (title-or-id function)
  "Execute FUNCTION with point at the node identified by TITLE-OR-ID.
FUNCTION receives the node as an argument.
Returns the result of FUNCTION."
  (let* ((node (if (and (stringp title-or-id)
                        (string-match-p "^[0-9a-f]\\{8\\}-" title-or-id))
                   (org-roam-node-from-id title-or-id)
                 (org-roam-node-from-title-or-alias title-or-id)))
         (file (when node (org-roam-node-file node)))
         (node-id (when node (org-roam-node-id node))))
    (unless node
      (error "Node not found: %s" title-or-id))
    (unless (file-exists-p file)
      (error "File not found: %s" file))

    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        ;; Search for the node's ID property
        (if (re-search-forward (format ":ID:[ \t]+%s" (regexp-quote node-id)) nil t)
            (progn
              ;; Move to the beginning of the entry
              (org-back-to-heading-or-point-min t)
              (funcall function node))
          (error "Could not locate node in file: %s" title-or-id))))))

(defun attach-file-to-note (title-or-id file-path)
  "Attach FILE-PATH to the org-roam note identified by TITLE-OR-ID.
Copies the file using org-attach. Returns the attachment directory path."
  (unless (file-exists-p file-path)
    (error "File does not exist: %s" file-path))

  (org-roam-skill--with-node-context
   title-or-id
   (lambda (node)
     ;; Attach the file using org-attach
     (org-attach-attach file-path nil 'cp)
     ;; Return info about the attachment
     (let ((attach-dir (org-attach-dir))
           (filename (file-name-nondirectory file-path)))
       (list :directory attach-dir
             :filename filename
             :full-path (expand-file-name filename attach-dir)
             :node-title (org-roam-node-title node))))))

(defun list-note-attachments (title-or-id)
  "List all attachments for the org-roam note identified by TITLE-OR-ID.
Returns a list of attachment info plists with :filename, :size, and :path."
  (org-roam-skill--with-node-context
   title-or-id
   (lambda (node)
     (let ((attach-dir (org-attach-dir)))
       (if (and attach-dir (file-exists-p attach-dir))
           (mapcar
            (lambda (filename)
              (let ((full-path (expand-file-name filename attach-dir)))
                (list :filename filename
                      :path full-path
                      :size (file-attribute-size (file-attributes full-path))
                      :modified (format-time-string
                                "%Y-%m-%d %H:%M:%S"
                                (file-attribute-modification-time
                                 (file-attributes full-path))))))
            (org-attach-file-list attach-dir))
         nil)))))

(defun delete-note-attachment (title-or-id filename)
  "Delete the attachment FILENAME from the note identified by TITLE-OR-ID.
Returns t on success, nil if attachment doesn't exist."
  (org-roam-skill--with-node-context
   title-or-id
   (lambda (node)
     (let* ((attach-dir (org-attach-dir))
            (attachments (when attach-dir (org-attach-file-list attach-dir))))
       (if (member filename attachments)
           (progn
             (org-attach-delete-one filename)
             t)
         (error "Attachment not found: %s" filename))))))

(defun get-attachment-path (title-or-id filename)
  "Get the full path to attachment FILENAME for note identified by TITLE-OR-ID.
Returns the full path if the attachment exists, nil otherwise."
  (org-roam-skill--with-node-context
   title-or-id
   (lambda (node)
     (let* ((attach-dir (org-attach-dir))
            (full-path (when attach-dir
                        (expand-file-name filename attach-dir))))
       (when (and full-path (file-exists-p full-path))
         full-path)))))

(defun get-note-attachment-dir (title-or-id)
  "Get the attachment directory for the note identified by TITLE-OR-ID.
Returns the directory path, or nil if no attachments exist."
  (org-roam-skill--with-node-context
   title-or-id
   (lambda (node)
     (org-attach-dir))))

(provide 'attach-file)
;;; attach-file.el ends here

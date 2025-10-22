;;; org-roam-skill-attach.el --- File attachment functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Tahir Butt
;; Keywords: outlines convenience

;;; Commentary:
;; Functions for attaching files to org-roam notes using org-attach.

;;; Code:

(require 'org-roam)
(require 'org-attach)
(require 'org-roam-skill-core)

;;;###autoload
(defun org-roam-skill-attach-file (title-or-id file-path)
  "Attach FILE-PATH to the org-roam note identified by TITLE-OR-ID.
Copy the file using org-attach.  Return the attachment directory path."
  (unless (file-exists-p file-path)
    (error "File does not exist: %s" file-path))

  (org-roam-skill--with-node-context
   title-or-id
   (lambda (node)
     ;; Attach the file using org-attach
     (org-attach-attach file-path nil 'cp)
     ;; Format the buffer after attaching (org-attach modifies PROPERTIES)
     (org-roam-skill--format-buffer)
     (save-buffer)
     ;; Return info about the attachment
     (let ((attach-dir (org-attach-dir))
           (filename (file-name-nondirectory file-path)))
       (list :directory attach-dir
             :filename filename
             :full-path (expand-file-name filename attach-dir)
             :node-title (org-roam-node-title node))))))

;;;###autoload
(defun org-roam-skill-list-attachments (title-or-id)
  "List all attachments for the org-roam note identified by TITLE-OR-ID.
Return a list of attachment info plists with :filename, :size, and :path."
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
                      :size (file-attribute-size
                             (file-attributes full-path))
                      :modified (format-time-string
                                "%Y-%m-%d %H:%M:%S"
                                (file-attribute-modification-time
                                 (file-attributes full-path))))))
            (org-attach-file-list attach-dir))
         nil)))))

;;;###autoload
(defun org-roam-skill-delete-attachment (title-or-id filename)
  "Delete the attachment FILENAME from the note identified by TITLE-OR-ID.
Return t on success, nil if attachment doesn't exist."
  (org-roam-skill--with-node-context
   title-or-id
   (lambda (node)
     (let* ((attach-dir (org-attach-dir))
            (attachments (when attach-dir (org-attach-file-list attach-dir))))
       (if (member filename attachments)
           (progn
             (org-attach-delete-one filename)
             ;; Format the buffer after deleting (may modify PROPERTIES)
             (org-roam-skill--format-buffer)
             (save-buffer)
             t)
         (error "Attachment not found: %s" filename))))))

;;;###autoload
(defun org-roam-skill-get-attachment-path (title-or-id filename)
  "Get the full path to attachment FILENAME for note TITLE-OR-ID.
Return the full path if the attachment exists, nil otherwise."
  (org-roam-skill--with-node-context
   title-or-id
   (lambda (node)
     (let* ((attach-dir (org-attach-dir))
            (full-path (when attach-dir
                        (expand-file-name filename attach-dir))))
       (when (and full-path (file-exists-p full-path))
         full-path)))))

;;;###autoload
(defun org-roam-skill-get-attachment-dir (title-or-id)
  "Get the attachment directory for the note identified by TITLE-OR-ID.
Return the directory path, or nil if no attachments exist."
  (org-roam-skill--with-node-context
   title-or-id
   (lambda (node)
     (org-attach-dir))))

(provide 'org-roam-skill-attach)
;;; org-roam-skill-attach.el ends here

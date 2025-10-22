;;; org-roam-skill-doctor.el --- Diagnostic functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Tahir Butt
;; Keywords: outlines convenience

;;; Commentary:
;; Diagnostic functions for checking org-roam setup and configuration.

;;; Code:

(require 'org-roam)
(require 'org-id)

;;;###autoload
(defun org-roam-doctor ()
  "Run diagnostic check on org-roam setup.
Returns a detailed report of the configuration status."
  (let ((checks '())
        (errors '())
        (warnings '()))

    ;; Check 1: org-roam loaded
    (if (featurep 'org-roam)
        (push "✓ org-roam is loaded" checks)
      (push "✗ org-roam is NOT loaded" errors))

    ;; Check 2: org-roam directory exists
    (if (and (boundp 'org-roam-directory)
             org-roam-directory
             (file-directory-p org-roam-directory))
        (push (format "✓ org-roam directory exists: %s" org-roam-directory) checks)
      (push (format "✗ org-roam directory not found or not set: %s"
                    (if (boundp 'org-roam-directory) org-roam-directory "NOT SET"))
            errors))

    ;; Check 3: org-roam directory is writable
    (when (and (boundp 'org-roam-directory)
               org-roam-directory
               (file-directory-p org-roam-directory))
      (if (file-writable-p org-roam-directory)
          (push "✓ org-roam directory is writable" checks)
        (push (format "✗ org-roam directory is NOT writable: %s" org-roam-directory) errors)))

    ;; Check 4: org-roam database exists
    (if (and (boundp 'org-roam-db-location)
             org-roam-db-location
             (file-exists-p org-roam-db-location))
        (push (format "✓ org-roam database exists: %s" org-roam-db-location) checks)
      (push (format "⚠ org-roam database not found (will be created on first sync): %s"
                    (if (boundp 'org-roam-db-location) org-roam-db-location "NOT SET"))
            warnings))

    ;; Check 5: Capture templates configured
    (if (and (boundp 'org-roam-capture-templates)
             org-roam-capture-templates)
        (let* ((default-template (assoc "d" org-roam-capture-templates)))
          (if default-template
              (progn
                (push (format "✓ Capture templates configured (%d template(s))"
                            (length org-roam-capture-templates)) checks)
                ;; Check template format using string representation
                (let ((template-str (format "%S" default-template)))
                  ;; Check filename format (informational only)
                  (cond
                   ((string-match-p "%<%Y%m%d%H%M%S>\\.org\"" template-str)
                    (push "✓ Using timestamp-only filename format (optimal for programmatic access)" checks))
                   ((string-match-p "%<.*>-\\${slug}" template-str)
                    (push "ℹ Using timestamp-slug format (will work, timestamp-only is cleaner)" checks))
                   ((string-match-p "\\${slug}" template-str)
                    (push "ℹ Using slug-based filename format (supported)" checks))
                   (t
                    (push "ℹ Custom filename format detected (should work if valid)" checks)))

                  ;; Check head content (informational only)
                  (cond
                   ((string-match-p "\"\\${title}\")" template-str)
                    (push "✓ Capture template uses ${title} for head content (recommended)" checks))
                   ((string-match-p "\"\")" template-str)
                    (push "✓ Capture template uses empty head content (org-roam adds title)" checks))
                   ((string-match-p "#\\+title: \\${title}" template-str)
                    (push "ℹ Template explicitly includes #+title directive (may duplicate)" checks))
                   (t
                    (push "ℹ Custom head content format detected" checks)))))
            (push "⚠ No default ('d') capture template found" warnings)))
      (push "⚠ org-roam-capture-templates not configured" warnings))

    ;; Check 6: Can query nodes
    (condition-case err
        (let ((node-count (length (org-roam-node-list))))
          (push (format "✓ Can query org-roam database (%d note(s) found)" node-count) checks))
      (error (push (format "✗ Error querying database: %s" (error-message-string err)) errors)))

    ;; Check 7: Database autosync mode
    (if (and (boundp 'org-roam-db-autosync-mode)
             org-roam-db-autosync-mode)
        (push "✓ Database autosync mode enabled" checks)
      (push "⚠ Database autosync mode not enabled (run: (org-roam-db-autosync-mode))" warnings))

    ;; Check 8: Required Elisp functions available
    (let ((required-functions '(org-roam-node-list
                               org-roam-node-from-title-or-alias
                               org-roam-node-from-id
                               org-roam-backlinks-get
                               org-roam-db-sync
                               org-id-uuid)))
      (dolist (func required-functions)
        (if (fboundp func)
            (push (format "✓ Function available: %s" func) checks)
          (push (format "✗ Required function NOT available: %s" func) errors))))

    ;; Generate report
    (with-temp-buffer
      (insert "==================================================\n")
      (insert "       ORG-ROAM DIAGNOSTIC REPORT\n")
      (insert "==================================================\n\n")

      ;; Summary
      (insert (format "Total checks: %d\n" (+ (length checks) (length warnings) (length errors))))
      (insert (format "Passed: %d\n" (length checks)))
      (insert (format "Warnings: %d\n" (length warnings)))
      (insert (format "Errors: %d\n\n" (length errors)))

      ;; Status
      (cond
       ((> (length errors) 0)
        (insert "Status: ✗ FAILED - Critical issues found\n\n"))
       ((> (length warnings) 0)
        (insert "Status: ⚠ WARNING - Setup works but has recommendations\n\n"))
       (t
        (insert "Status: ✓ PASSED - org-roam is properly configured\n\n")))

      ;; Passed checks
      (when checks
        (insert "PASSED CHECKS:\n")
        (insert "----------------------------------------\n")
        (dolist (check (nreverse checks))
          (insert (format "%s\n" check)))
        (insert "\n"))

      ;; Warnings
      (when warnings
        (insert "WARNINGS:\n")
        (insert "----------------------------------------\n")
        (dolist (warning (nreverse warnings))
          (insert (format "%s\n" warning)))
        (insert "\n"))

      ;; Errors
      (when errors
        (insert "ERRORS:\n")
        (insert "----------------------------------------\n")
        (dolist (error (nreverse errors))
          (insert (format "%s\n" error)))
        (insert "\n"))

      ;; Configuration details
      (insert "CONFIGURATION DETAILS:\n")
      (insert "----------------------------------------\n")
      (insert (format "Emacs version: %s\n" emacs-version))
      (insert (format "org-roam directory: %s\n"
                     (if (boundp 'org-roam-directory) org-roam-directory "NOT SET")))
      (insert (format "org-roam database: %s\n"
                     (if (boundp 'org-roam-db-location) org-roam-db-location "NOT SET")))
      (insert (format "Database size: %s\n"
                     (if (and (boundp 'org-roam-db-location)
                             org-roam-db-location
                             (file-exists-p org-roam-db-location))
                         (format "%d bytes" (nth 7 (file-attributes org-roam-db-location)))
                       "N/A")))
      (insert "\n")
      (insert "==================================================\n")

      (buffer-string))))

;;;###autoload
(defun org-roam-doctor-and-print ()
  "Run org-roam diagnostic and print the report.
Use this function when calling from emacsclient."
  (let ((report (org-roam-doctor)))
    (message "%s" report)
    report))

;;;###autoload
(defun org-roam-doctor-quick ()
  "Quick diagnostic check - return t if setup is OK, nil otherwise.
Returns a simple pass/fail status."
  (and (featurep 'org-roam)
       (boundp 'org-roam-directory)
       org-roam-directory
       (file-directory-p org-roam-directory)
       (file-writable-p org-roam-directory)
       (condition-case nil
           (progn (org-roam-node-list) t)
         (error nil))))

(provide 'org-roam-skill-doctor)
;;; org-roam-skill-doctor.el ends here

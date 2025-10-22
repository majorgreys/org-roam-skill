;;; org-roam-skill-test.el --- Unit tests for org-roam-skill -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for org-roam-skill functions using Buttercup

;;; Code:

(require 'buttercup)
(require 'org-roam-skill)

;;; Tag Sanitization Tests

(describe "org-roam-skill--sanitize-tag"
  (it "replaces hyphens with underscores"
    (expect (org-roam-skill--sanitize-tag "my-tag") :to-equal "my_tag"))

  (it "handles multi-word tags"
    (expect (org-roam-skill--sanitize-tag "multi-word-tag") :to-equal "multi_word_tag"))

  (it "leaves already clean tags unchanged"
    (expect (org-roam-skill--sanitize-tag "already_clean") :to-equal "already_clean")
    (expect (org-roam-skill--sanitize-tag "no_change") :to-equal "no_change")))

;;; Filename Generation Tests

(describe "org-roam-skill--expand-filename"
  (it "generates timestamp-only filenames"
    (let ((org-roam-capture-templates
           '(("d" "default" plain "%?"
              :target (file+head "%<%Y%m%d%H%M%S>.org" "${title}")
              :unnarrowed t))))
      (let ((filename (org-roam-skill--expand-filename "Test Note")))
        (expect filename :to-match "^[0-9]\\{14\\}\\.org$"))))

  (it "generates timestamp-slug filenames"
    (let ((org-roam-capture-templates
           '(("d" "default" plain "%?"
              :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "${title}")
              :unnarrowed t))))
      (let ((filename (org-roam-skill--expand-filename "Test Note")))
        (expect filename :to-match "^[0-9]\\{14\\}-test_note\\.org$")))))

;;; Doctor Functions Tests

(describe "org-roam-doctor-quick"
  (it "returns status of org-roam setup"
    (let ((org-roam-directory (make-temp-file "org-roam-test-" t))
          (org-roam-db-location (expand-file-name "org-roam.db"
                                                   (make-temp-file "org-roam-test-" t))))
      (unwind-protect
          (progn
            (org-roam-db-sync)
            (expect (org-roam-doctor-quick) :to-be t))
        (when (file-exists-p org-roam-directory)
          (delete-directory org-roam-directory t))))))

(describe "check-org-roam-setup"
  (it "returns setup information"
    (let ((org-roam-directory (make-temp-file "org-roam-test-" t))
          (org-roam-db-location (expand-file-name "org-roam.db"
                                                   (make-temp-file "org-roam-test-" t))))
      (unwind-protect
          (progn
            (org-roam-db-sync)
            (let ((setup (org-roam-skill-check-setup)))
              (expect setup :not :to-be nil)
              (expect (plist-get setup :org-roam-loaded) :to-be t)
              (expect (plist-get setup :directory-exists) :to-be t)))
        (when (file-exists-p org-roam-directory)
          (delete-directory org-roam-directory t))))))

(provide 'org-roam-skill-test)
;;; org-roam-skill-test.el ends here

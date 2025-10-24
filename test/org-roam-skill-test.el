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

;;; Time Format Expansion Tests

(describe "org-roam-skill--expand-time-formats"
  (it "expands custom time format %<...>"
    (let ((result (org-roam-skill--expand-time-formats "Date: %<%Y-%m-%d>")))
      (expect result :to-match "Date: [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")))

  (it "expands %U inactive timestamp with time"
    (let ((result (org-roam-skill--expand-time-formats "Created: %U")))
      (expect result :to-match "Created: \\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Z][a-z][a-z] [0-9]\\{2\\}:[0-9]\\{2\\}\\]")))

  (it "expands %u inactive timestamp without time"
    (let ((result (org-roam-skill--expand-time-formats "Date: %u")))
      (expect result :to-match "Date: \\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Z][a-z][a-z]\\]")))

  (it "expands %T active timestamp with time"
    (let ((result (org-roam-skill--expand-time-formats "Scheduled: %T")))
      (expect result :to-match "Scheduled: <[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Z][a-z][a-z] [0-9]\\{2\\}:[0-9]\\{2\\}>")))

  (it "expands %t active timestamp without time"
    (let ((result (org-roam-skill--expand-time-formats "Deadline: %t")))
      (expect result :to-match "Deadline: <[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Z][a-z][a-z]>")))

  (it "expands multiple time formats in one string"
    (let ((result (org-roam-skill--expand-time-formats "#+date: %<%Y-%m-%d>\n#+created: %U")))
      (expect result :to-match "^#\\+date: [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
      (expect result :to-match "#\\+created: \\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")))

  (it "leaves text without time formats unchanged"
    (let ((result (org-roam-skill--expand-time-formats "Just plain text")))
      (expect result :to-equal "Just plain text"))))

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

;;; Content Formatting Tests

(describe "org-roam-skill--detect-format"
  (it "detects org headings"
    (expect (org-roam-skill--detect-format "* Heading\n\nContent") :to-be 'org))

  (it "detects org blocks"
    (expect (org-roam-skill--detect-format "#+begin_src python\ncode\n#+end_src") :to-be 'org))

  (it "detects org properties"
    (expect (org-roam-skill--detect-format ":PROPERTIES:\n:ID: 123\n:END:") :to-be 'org))

  (it "detects org italic emphasis"
    (expect (org-roam-skill--detect-format "Some /italic/ text") :to-be 'org))

  (it "defaults to markdown for plain text"
    (expect (org-roam-skill--detect-format "Just plain text") :to-be 'markdown))

  (it "defaults to markdown for markdown headings"
    (expect (org-roam-skill--detect-format "# Markdown heading") :to-be 'markdown)))

(describe "org-roam-skill--format-content"
  (it "converts markdown headings to org headings"
    (let ((input "# Heading 1\n\nSome text."))
      (expect (org-roam-skill--format-content input) :to-match "\\* Heading 1")))

  (it "converts markdown bold to org bold"
    (let ((input "Some **bold** text."))
      (expect (org-roam-skill--format-content input) :to-match "\\*bold\\*")))

  (it "converts markdown italic to org italic"
    (let ((input "Some *italic* text."))
      (expect (org-roam-skill--format-content input) :to-match "/italic/")))

  (it "converts markdown code blocks to org src blocks"
    (let ((input "```python\ndef test():\n    pass\n```"))
      (expect (org-roam-skill--format-content input) :to-match "#\\+begin_src python")
      (expect (org-roam-skill--format-content input) :to-match "#\\+end_src")))

  (it "normalizes org-mode content"
    (let ((input "* Heading\n\nSome *bold* text."))
      ;; Should detect org format and preserve heading structure
      (expect (org-roam-skill--format-content input) :to-match "\\* Heading")
      (expect (org-roam-skill--format-content input) :to-match "\\*bold\\*")))

  (it "skips formatting when NO-FORMAT is t"
    (let ((input "# Markdown heading"))
      (expect (org-roam-skill--format-content input t) :to-equal input)))

  (it "skips formatting when content starts with NO_FORMAT:"
    (let ((input "NO_FORMAT:# Raw markdown"))
      (expect (org-roam-skill--format-content input) :to-equal "# Raw markdown")))

  (it "returns empty string for nil content"
    (expect (org-roam-skill--format-content nil) :to-be nil))

  (it "returns empty string for empty content"
    (expect (org-roam-skill--format-content "") :to-equal ""))

  (it "removes CUSTOM_ID properties added by pandoc"
    (let ((input "# Test"))
      (expect (org-roam-skill--format-content input) :not :to-match "CUSTOM_ID"))))

;;; Formatting Tests

(describe "org-roam-skill--format-buffer"
  (it "aligns tables in org buffer with proper pipe alignment"
    (with-temp-buffer
      (org-mode)
      (insert "| Name | Value |\n")
      (insert "| Foo | Bar |\n")
      (insert "| LongName | ShortVal |\n")
      (org-roam-skill--format-buffer)
      (goto-char (point-min))
      (expect (org-at-table-p) :to-be t)
      ;; Check that pipes are aligned
      (let ((output (substring-no-properties (buffer-string))))
        (expect output :to-equal "| Name     | Value    |\n| Foo      | Bar      |\n| LongName | ShortVal |\n")))))

(describe "format-org-roam-note"
  (it "formats an org-roam note file with aligned table pipes"
    (let* ((org-roam-directory (make-temp-file "org-roam-test-" t))
           (org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))
           (test-id (org-id-uuid))
           (test-file (expand-file-name "test-note.org" org-roam-directory)))
      (unwind-protect
          (progn
            ;; Create a test note with an unaligned table
            (with-temp-file test-file
              (insert ":PROPERTIES:\n")
              (insert (format ":ID:       %s\n" test-id))
              (insert ":END:\n")
              (insert "#+title: Test Note\n\n")
              (insert "| Name | Value |\n")
              (insert "| Foo | Bar |\n")
              (insert "| LongName | ShortVal |\n"))
            (org-roam-db-sync)
            ;; Format the note
            (expect (format-org-roam-note test-id) :to-be t)
            ;; Verify file was formatted with aligned pipes
            (with-temp-buffer
              (insert-file-contents test-file)
              (let ((content (buffer-string)))
                (expect content :to-match "| Name")
                ;; Verify pipes are aligned (all rows have same pipe positions)
                (expect content :to-match "| Name     | Value    |")
                (expect content :to-match "| Foo      | Bar      |")
                (expect content :to-match "| LongName | ShortVal |"))))
        (when (file-exists-p org-roam-directory)
          (delete-directory org-roam-directory t))))))

(provide 'org-roam-skill-test)
;;; org-roam-skill-test.el ends here

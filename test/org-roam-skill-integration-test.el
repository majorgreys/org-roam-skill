;;; org-roam-skill-integration-test.el --- Integration tests for org-roam-skill -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests for org-roam-skill that test complete workflows using Buttercup

;;; Code:

(require 'buttercup)
(require 'org-roam-skill)
(require 'test-helper)

;;; Integration Test Suite with Shared Setup/Teardown

(describe "org-roam-skill integration tests"

  (before-each
    (org-roam-skill-test--setup))

  (after-each
    (org-roam-skill-test--teardown))

  ;;; Note Creation Tests

  (describe "create-org-roam-note"
    (it "creates a basic note"
      (let ((file-path (org-roam-skill-create-note "Test Note" '("test" "integration"))))
        (expect (file-exists-p file-path) :to-be-truthy)
        (expect (org-roam-skill-test--node-exists-p "Test Note") :to-be-truthy)
        (expect (org-roam-skill-test--count-nodes) :to-equal 1)
        ;; Check file content
        (let ((content (org-roam-skill-test--get-note-content file-path)))
          (expect (string-match-p ":ID:" content) :to-be-truthy)
          (expect (string-match-p "#\\+title: Test Note" content) :to-be-truthy)
          (expect (string-match-p ":test:integration:" content) :to-be-truthy))))

    (it "creates a note with content"
      (let ((file-path (org-roam-skill-create-note "Test Note" '("test") "Some content here")))
        (expect (file-exists-p file-path) :to-be-truthy)
        (let ((content (org-roam-skill-test--get-note-content file-path)))
          (expect content :to-match "Some content here"))))

    (it "sanitizes tags with hyphens"
      (let ((file-path (org-roam-skill-create-note "Test Note" '("my-tag" "another-tag"))))
        (let ((content (org-roam-skill-test--get-note-content file-path)))
          (expect (string-match-p ":my_tag:another_tag:" content) :to-be-truthy)
          (expect (string-match-p "my-tag" content) :not :to-be-truthy)))))

  ;;; Search Tests

  (describe "search-notes-by-title"
    (it "finds notes by title"
      (org-roam-skill-create-note "First Note" '("test"))
      (org-roam-skill-create-note "Second Note" '("test"))
      (org-roam-skill-create-note "Another Topic" '("other"))
      (let ((results (org-roam-skill-search-by-title "Note")))
        (expect (length results) :to-equal 2))))

  (describe "search-notes-by-tag"
    (it "finds notes by tag"
      (org-roam-skill-create-note "Note 1" '("test" "project"))
      (org-roam-skill-create-note "Note 2" '("test"))
      (org-roam-skill-create-note "Note 3" '("other"))
      (let ((results (org-roam-skill-search-by-tag "test")))
        (expect (length results) :to-equal 2))))

  (describe "get-node-by-title"
    (it "retrieves node by exact title"
      (org-roam-skill-create-note "Exact Match" '("test"))
      (let ((node (org-roam-skill-get-node-by-title "Exact Match")))
        (expect node :not :to-be nil)
        (expect (plist-get node :title) :to-equal "Exact Match"))))

  (describe "search for nonexistent note"
    (it "returns empty results"
      (org-roam-skill-create-note "Existing Note" '("test"))
      (let ((results (org-roam-skill-search-by-title "Nonexistent")))
        (expect results :to-equal nil))))

  ;;; Link Tests

  (describe "create-bidirectional-link"
    (it "creates links between notes"
      (org-roam-skill-create-note "Source Note" '("test"))
      (org-roam-skill-create-note "Target Note" '("test"))
      (org-roam-skill-create-bidirectional-link "Source Note" "Target Note")
      ;; Sync database to register the link
      (org-roam-db-sync)
      (let ((backlinks (org-roam-skill-get-backlinks-by-title "Target Note")))
        (expect (>= (length backlinks) 1) :to-be-truthy))))

  (describe "get-forward-links"
    (it "retrieves forward links"
      (org-roam-skill-create-note "Source" '("test"))
      (org-roam-skill-create-note "Target" '("test"))
      (org-roam-skill-create-bidirectional-link "Source" "Target")
      (let ((links (org-roam-skill-get-forward-links-by-title "Source")))
        (expect links :not :to-be nil))))

  (describe "backlinks for nonexistent note"
    (it "returns empty list"
      (let ((backlinks (org-roam-skill-get-backlinks-by-title "Nonexistent")))
        (expect backlinks :to-equal nil))))

  ;;; Tag Management Tests

  (describe "list-all-tags"
    (it "lists all unique tags"
      (org-roam-skill-create-note "Note 1" '("tag1" "tag2"))
      (org-roam-skill-create-note "Note 2" '("tag2" "tag3"))
      (let ((tags (org-roam-skill-list-all-tags)))
        (expect (>= (length tags) 3) :to-be-truthy))))

  (describe "count-notes-by-tag"
    (it "counts notes with specific tag"
      (org-roam-skill-create-note "Note 1" '("test"))
      (org-roam-skill-create-note "Note 2" '("test" "other"))
      (org-roam-skill-create-note "Note 3" '("other"))
      (let ((counts (org-roam-skill-count-notes-by-tag)))
        (expect (assoc "test" counts) :not :to-be nil)
        (expect (cdr (assoc "test" counts)) :to-equal 2))))

  (describe "add-tag-to-note"
    (it "adds tag to existing note"
      (let ((file-path (org-roam-skill-create-note "Test Note" '("initial"))))
        (org-roam-skill-add-tag "Test Note" "added")
        (let ((content (org-roam-skill-test--get-note-content file-path)))
          (expect content :to-match ":added:")))))

  (describe "get-notes-without-tags"
    (it "finds untagged notes"
      (org-roam-skill-create-note "Tagged Note" '("tag"))
      (org-roam-skill-create-note "Untagged Note" nil)
      (let ((results (org-roam-skill-get-notes-without-tags)))
        (expect (length results) :to-equal 1))))

  ;;; Utility Function Tests

  (describe "find-orphan-notes"
    (it "finds notes without links"
      (org-roam-skill-create-note "Connected" '("test"))
      (org-roam-skill-create-note "Orphan" '("test"))
      (org-roam-skill-create-note "Another Connected" '("test"))
      (org-roam-skill-create-bidirectional-link "Connected" "Another Connected")
      (let ((orphans (org-roam-skill-find-orphan-notes)))
        (expect (>= (length orphans) 1) :to-be-truthy))))

  (describe "list-recent-notes"
    (it "lists recently created notes"
      (org-roam-skill-create-note "Note 1" '("test"))
      (org-roam-skill-create-note "Note 2" '("test"))
      (org-roam-skill-create-note "Note 3" '("test"))
      (let ((recent (org-roam-skill-list-recent-notes 2)))
        (expect (length recent) :to-equal 2))))

  (describe "get-graph-stats"
    (it "returns graph statistics"
      (org-roam-skill-create-note "Note 1" '("test"))
      (org-roam-skill-create-note "Note 2" '("test"))
      (org-roam-skill-create-bidirectional-link "Note 1" "Note 2")
      (let ((stats (org-roam-skill-get-graph-stats)))
        (expect stats :not :to-be nil)
        (expect (plist-get stats :total-notes) :to-equal 2))))

  ;;; Edge Cases

  (describe "empty database"
    (it "handles empty database gracefully"
      (expect (org-roam-skill-test--count-nodes) :to-equal 0)
      (expect (org-roam-skill-list-all-tags) :to-equal nil)
      (expect (org-roam-skill-search-by-title "anything") :to-equal nil))))

(provide 'org-roam-skill-integration-test)
;;; org-roam-skill-integration-test.el ends here

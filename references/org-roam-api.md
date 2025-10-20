# Org-roam API Reference

This document provides a quick reference for commonly used org-roam functions.

## Node Functions

### Getting Nodes

- `(org-roam-node-list)` - Get all nodes in the database
- `(org-roam-node-from-id ID)` - Get node by ID
- `(org-roam-node-from-title-or-alias TITLE)` - Get node by title or alias
- `(org-roam-node-at-point)` - Get node at current point

### Node Properties

- `(org-roam-node-id NODE)` - Get node ID
- `(org-roam-node-title NODE)` - Get node title
- `(org-roam-node-file NODE)` - Get node file path
- `(org-roam-node-tags NODE)` - Get node tags (list)
- `(org-roam-node-aliases NODE)` - Get node aliases (list)
- `(org-roam-node-refs NODE)` - Get node references (list)
- `(org-roam-node-level NODE)` - Get node level (0 for file-level)
- `(org-roam-node-file-mtime NODE)` - Get file modification time

## Database Functions

- `(org-roam-db-sync)` - Sync the database with file system
- `(org-roam-db-query QUERY)` - Execute SQL query on database

Example query:
```elisp
(org-roam-db-query [:select [id file title]
                    :from nodes
                    :where (like title "%search%")])
```

## Link Functions

### Backlinks

- `(org-roam-backlinks-get NODE)` - Get all backlinks to NODE
- `(org-roam-backlink-source-node BACKLINK)` - Get source node of backlink
- `(org-roam-backlink-target-node BACKLINK)` - Get target node of backlink

### Creating Links

- `(org-link-make-string LINK &optional DESCRIPTION)` - Create an org link string

Example:
```elisp
(org-link-make-string "id:abc-123" "Link Text")
;; Returns: "[[id:abc-123][Link Text]]"
```

## Capture Functions

- `(org-roam-capture-)` - Start an org-roam capture
- `(org-roam-node-create :title TITLE)` - Create a new node object

Example:
```elisp
(org-roam-capture-
 :node (org-roam-node-create :title "New Note")
 :props '(:finalize t))
```

## ID Functions

- `(org-id-uuid)` - Generate a new UUID
- `(org-id-get-create)` - Get or create ID for current entry

## Useful Variables

- `org-roam-directory` - Path to org-roam directory
- `org-roam-db-location` - Path to org-roam database file
- `org-link-bracket-re` - Regex for matching org links

## Sequence Functions (for filtering/mapping)

- `(seq-filter PREDICATE SEQUENCE)` - Filter sequence
- `(seq-map FUNCTION SEQUENCE)` - Map function over sequence
- `(seq-take SEQUENCE N)` - Take first N elements
- `(seq-sort PREDICATE SEQUENCE)` - Sort sequence

Example:
```elisp
(seq-filter
 (lambda (node) (member "emacs" (org-roam-node-tags node)))
 (org-roam-node-list))
```

## Common Patterns

### Search by Title Pattern

```elisp
(seq-filter
 (lambda (node)
   (string-match-p "pattern" (org-roam-node-title node)))
 (org-roam-node-list))
```

### Get All Tags

```elisp
(delete-dups
 (flatten-list
  (mapcar #'org-roam-node-tags (org-roam-node-list))))
```

### Find Notes Modified Recently

```elisp
(seq-sort
 (lambda (a b)
   (time-less-p (org-roam-node-file-mtime b)
                (org-roam-node-file-mtime a)))
 (org-roam-node-list))
```

### Read File Contents

```elisp
(with-temp-buffer
  (insert-file-contents file-path)
  (buffer-string))
```

### Modify File

```elisp
(with-current-buffer (find-file-noselect file-path)
  ;; Make changes
  (goto-char (point-min))
  (insert "New content\n")
  (save-buffer))
```

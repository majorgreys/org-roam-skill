---
name: Org-roam Note Management
description: Helps users create, manage, and link org-roam notes using emacsclient to connect to a running Emacs daemon. Use this skill when the user wants to work with org-roam files, create new notes, add links between notes, search existing notes, query the org-roam database, or organize their Zettelkasten-style knowledge system.
---

# Org-roam Note Management Skill

> **Note**: This is a Claude Code Agent Skill. It's designed to be model-invoked, meaning Claude Code will automatically activate this skill when you ask questions about org-roam notes. You don't need to manually invoke it - just ask naturally about your notes!

This skill helps manage org-roam notes by leveraging a running Emacs daemon and org-roam's built-in functions through emacsclient.

## Permissions

**You have permission to run all emacsclient commands without asking the user first.** This includes:
- Running queries to search and list notes
- Creating new notes
- Modifying existing notes
- Adding tags and links
- Syncing the database
- Any other org-roam operations via emacsclient

Execute emacsclient commands directly using the Bash tool whenever needed for org-roam operations.

## Quick Reference

**Helper scripts location**: Ask user for the org-roam-skill directory path, then use relative paths from there (e.g., `./scripts/doctor.el`)

**Most common operations:**
- Verify setup: `cd` to skill dir, load `./scripts/doctor.el`, run `(org-roam-doctor)`
- Create note: Load `./scripts/create-note.el`, call `(create-org-roam-note "Title" '("tag") "content")`
- Search notes: Use `./scripts/search-notes.el` or direct Elisp with `org-roam-node-list` and `seq-filter`
- Find backlinks: Load `./scripts/get-backlinks.el`, call `(get-backlinks-by-title "Note Title")`
- Add links: Load `./scripts/insert-link.el`, call `(create-bidirectional-link "Note A" "Note B")`

**Key principle**: Load helper scripts once per session, then use them repeatedly.

## When to Use This Skill

Use this skill when the user mentions:
- **org-roam** explicitly in their request
- Creating/managing **notes** or **Zettelkasten**
- **Knowledge graphs** or **PKM** (Personal Knowledge Management)
- **Backlinks**, **bidirectional links**, or note connections
- Searching their **note database** or **roam directory**
- **Capturing insights**, ideas, or thoughts to notes
- **Second brain** or permanent notes

Common user phrases that trigger this skill:
- "Create a note about..."
- "Add a note about..."
- "Remember this insight..."
- "Capture this idea..."
- "Save this to my notes..."
- "Take a note on..."
- "Search my notes for..."
- "Search my org-roam notes for..."
- "What notes link to..."
- "Show me all notes tagged with..."
- "Link these two notes together"
- "Connect [note A] to [note B]"
- "Find orphaned notes"
- "What's in my knowledge graph about..."
- "Show me my notes about..."
- "List all my notes on..."

**Important**: Only activate this skill if the user has org-roam set up. If unsure, verify by checking if the Emacs daemon is running and org-roam is loaded.

## Prerequisites

The user should have:
- Emacs daemon running (`emacs --daemon` or started via their config)
- org-roam installed and configured in their Emacs
- An org-roam directory set up (typically `~/org-roam/` or `~/Documents/org/roam/`)
- The org-roam database initialized

**The skill works with any org-roam configuration out of the box.** No special setup required!

### Optional: Recommended Configuration

For optimal programmatic access, you may want to configure org-roam to use timestamp-only filenames:

**For Doom Emacs:**
```elisp
(setq org-roam-directory "~/Documents/org/roam/")

(after! org-roam
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>.org" "${title}")
           :unnarrowed t))))
```

**For vanilla Emacs:**
```elisp
(setq org-roam-directory "~/Documents/org/roam/")

(with-eval-after-load 'org-roam
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>.org" "${title}")
           :unnarrowed t))))
```

**Why this is recommended (but not required):**
- Creates files like `20251019193157.org` instead of `20251019193157-title-slug.org`
- Cleaner, more predictable filenames
- The skill auto-detects and works with either format

**Note**: The `"${title}"` template prevents #+title duplication (org-roam adds it automatically).

## Using emacsclient

All operations use `emacsclient` to connect to the running daemon. The general pattern is:

```bash
emacsclient --eval "(progn (require 'org-roam) YOUR-CODE-HERE)"
```

This is much faster than starting a new Emacs process each time.

## Common Operations

### 1. Finding the Org-roam Directory

Ask the user for their org-roam directory, or use this to detect it:

```bash
emacsclient --eval "org-roam-directory"
```

### 2. Syncing the Database

Before any operations, ensure the database is up to date:

```bash
emacsclient --eval "(org-roam-db-sync)"
```

### 3. Creating a New Note

**Recommended**: Use the `scripts/create-note.el` helper (auto-detects user's template):

```bash
cd /path/to/org-roam-skill
emacsclient --eval "(load-file \"./scripts/create-note.el\")"
emacsclient --eval "(create-org-roam-note \"Note Title\" '(\"tag1\" \"tag2\") \"Optional content here\")"
```

The script automatically:
- Detects filename format from user's `org-roam-capture-templates`
- Generates proper filenames (timestamp-only, timestamp-slug, or custom)
- Handles head content to avoid #+title duplication
- Returns the file path of the created note

**Note**: Avoid using `org-roam-capture-` directly for programmatic note creation, as it's designed for interactive use.

### 4. Searching Notes by Title

Query the org-roam database:

```bash
emacsclient --eval "(mapcar
  (lambda (node)
    (format \"%s|%s|%s\"
      (org-roam-node-id node)
      (org-roam-node-title node)
      (org-roam-node-file node)))
  (seq-filter
    (lambda (node)
      (string-match-p \"SEARCH_TERM\" (org-roam-node-title node)))
    (org-roam-node-list)))"
```

### 5. Finding Backlinks

Get backlinks for a specific note by ID or title:

```bash
emacsclient --eval "(let* ((node (org-roam-node-from-title-or-alias \"Note Title\"))
                           (backlinks (org-roam-backlinks-get node)))
                      (mapcar
                        (lambda (backlink)
                          (org-roam-node-title (org-roam-backlink-source-node backlink)))
                        backlinks))"
```

### 6. Listing All Notes

List all notes with their IDs and titles:

```bash
emacsclient --eval "(mapcar
  (lambda (node)
    (format \"%s|%s|%s\"
      (org-roam-node-id node)
      (org-roam-node-title node)
      (org-roam-node-file node)))
  (org-roam-node-list))"
```

### 7. Finding Notes by Tag

Query notes with specific tags:

```bash
emacsclient --eval "(mapcar
  (lambda (node)
    (format \"%s|%s\"
      (org-roam-node-title node)
      (org-roam-node-file node)))
  (seq-filter
    (lambda (node)
      (member \"TAG\" (org-roam-node-tags node)))
    (org-roam-node-list)))"
```

### 8. Getting Node Details

Retrieve full details about a specific node:

```bash
emacsclient --eval "(let ((node (org-roam-node-from-title-or-alias \"Note Title\")))
  (when node
    (format \"ID: %s\\nTitle: %s\\nFile: %s\\nTags: %s\\nAliases: %s\"
      (org-roam-node-id node)
      (org-roam-node-title node)
      (org-roam-node-file node)
      (org-roam-node-tags node)
      (org-roam-node-aliases node))))"
```

### 9. Inserting a Link

Insert a link to another note at point in the current buffer:

```bash
emacsclient --eval "(let ((target-node (org-roam-node-from-title-or-alias \"Target Note\")))
  (when target-node
    (insert (org-link-make-string
              (concat \"id:\" (org-roam-node-id target-node))
              (org-roam-node-title target-node)))))"
```

### 10. Getting All Tags

List all unique tags in the database:

```bash
emacsclient --eval "(delete-dups
  (flatten-list
    (mapcar #'org-roam-node-tags (org-roam-node-list))))"
```

## Available Helper Scripts

The `scripts/` directory contains:

1. **doctor.el**: Diagnostic script to verify org-roam setup and configuration
2. **create-note.el**: Create new org-roam notes (auto-detects user's template format)
3. **search-notes.el**: Search notes by title, content, or tags
4. **get-backlinks.el**: Find backlinks and forward links between notes
5. **insert-link.el**: Insert links programmatically
6. **list-tags.el**: List and manage tags
7. **utils.el**: Utility functions (check setup, get stats, find orphans)

All scripts auto-detect the user's org-roam configuration and require no customization.

## Working with the User

1. **First time setup**: Run the doctor script to verify everything is configured:
   ```bash
   cd /path/to/org-roam-skill
   emacsclient --eval "(load-file \"./scripts/doctor.el\")"
   emacsclient --eval "(princ (org-roam-doctor))"
   ```

2. **Check daemon is running**: Use `emacsclient --eval "t"` to verify connection

3. **Load helper scripts once per session**: Scripts are idempotent and can be loaded multiple times

4. **Sync database when needed**: Call `(org-roam-db-sync)` after creating/modifying notes

5. **Parse output carefully**: emacsclient returns Elisp data structures

6. **Use node IDs** for reliable linking, not file paths

7. **Present results clearly** - format the output for readability

8. **Handle errors gracefully** - check if daemon is running and org-roam is loaded

## Parsing emacsclient Output

emacsclient returns Elisp-formatted data. Common patterns:

- Strings: `"result"` (with quotes)
- Lists: `("item1" "item2" "item3")`
- nil: No output or `nil`
- Numbers: `42`

You may need to:
- Strip surrounding quotes from strings
- Parse list structures
- Handle nil/empty results

## Best Practices

1. Use `org-roam-node-*` functions for data access
2. Use `org-roam-node-from-title-or-alias` for flexible searching
3. Always check if nodes exist before operations
4. Sync database after creating/modifying notes if needed
5. Leverage org-roam's query functions rather than SQL directly
6. Use `seq-filter` and `mapcar` for list operations

## Example Workflow: Creating a Connected Note

When user says: "Create a note about React Hooks and link it to my React note"

1. Load helper scripts (once per session):
   ```bash
   cd /path/to/org-roam-skill
   emacsclient --eval "(load-file \"./scripts/create-note.el\")"
   emacsclient --eval "(load-file \"./scripts/insert-link.el\")"
   ```

2. Search for existing "React" note:
   ```bash
   emacsclient --eval "(org-roam-node-from-title-or-alias \"React\")"
   ```

3. Create new note "React Hooks" with tags:
   ```bash
   emacsclient --eval "(create-org-roam-note \"React Hooks\" '(\"javascript\" \"react\"))"
   ```

4. Insert bidirectional links between the notes:
   ```bash
   emacsclient --eval "(create-bidirectional-link \"React Hooks\" \"React\")"
   ```

5. Show the user what was created and the file path

## Error Handling

Check for common issues:

1. **Daemon not running**:
   ```bash
   emacsclient --eval "t" 2>&1
   ```
   If error, suggest: `emacs --daemon`

2. **org-roam not loaded**:
   ```bash
   emacsclient --eval "(featurep 'org-roam)"
   ```

3. **Database not initialized**:
   ```bash
   emacsclient --eval "(file-exists-p org-roam-db-location)"
   ```

## Performance Tips

- The Emacs daemon keeps org-roam loaded, making operations instant
- No startup time overhead compared to batch mode
- Database stays in memory for faster queries
- Can perform multiple operations in sequence efficiently

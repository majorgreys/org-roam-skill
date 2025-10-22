---
name: Org-roam Note Management
description: |
  Helps users create, manage, and link org-roam notes using emacsclient to connect to a running Emacs daemon.

  **ALWAYS USE THIS SKILL** when user mentions "roam note" or "org-roam", references file paths containing `/roam/` or `/org-roam/`, or wants to create/search/link notes in their roam directory.

  Use this skill for: creating roam notes, searching notes, adding backlinks, querying org-roam database, managing Zettelkasten-style knowledge systems.

  **NEVER use Read/Write/Edit tools directly on roam notes** - they bypass database sync and break org-roam functionality.
---

# Org-roam Note Management Skill

> **Note**: This is a Claude Code Agent Skill. It's designed to be model-invoked, meaning Claude Code will automatically activate this skill when you ask questions about org-roam notes. You don't need to manually invoke it - just ask naturally about your notes!

## ⚠️ CRITICAL: NEVER Use Direct File Tools for Roam Notes

**ALWAYS invoke this skill instead of using Read/Write/Edit tools directly** when:
- User mentions "roam note" or "org-roam note" (with or without "org-" prefix)
- File paths contain `/roam/`, `/org-roam/`, or `org-roam` directory patterns
- User wants to create, modify, or query notes in their roam directory
- User references existing roam notes by path (e.g., `~/Documents/org/roam/20251020203000.org`)

**Why this matters:**
- Roam notes require proper org-roam database updates
- IDs must be generated with microseconds precision using emacsclient
- File creation must respect user's org-roam-capture-templates
- Direct file operations bypass database sync and break backlinks

**If you catch yourself about to use Write/Edit/Read on a roam note: STOP and invoke this skill first.**

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

**Setup requirement**: User must have loaded `org-roam-skill` in their Emacs config. Verify with:
```bash
emacsclient --eval "(featurep 'org-roam-skill)"
```

If not loaded, ask user to add to their Emacs config (see installation instructions below).

**Most common operations** (all functions are already in memory):
- Verify setup: `emacsclient --eval "(org-roam-doctor)"`
- Create note: `emacsclient --eval "(create-org-roam-note \"Title\" '(\"tag\") \"content\")"`
- Search notes: `emacsclient --eval "(search-notes-by-title \"search-term\")"`
- Find backlinks: `emacsclient --eval "(get-backlinks-by-title \"Note Title\")"`
- Add links: `emacsclient --eval "(create-bidirectional-link \"Note A\" \"Note B\")"`
- Attach files: `emacsclient --eval "(attach-file-to-note \"Note Title\" \"/path/to/file\")"`

**Key principle**: Functions are loaded once at Emacs startup - no repeated loading overhead.

## When to Use This Skill

**HIGH PRIORITY TRIGGERS** - Invoke this skill immediately when:

### Explicit Keywords (case-insensitive)
- User says "**roam note**" or "**org-roam**" (with or without "org-" prefix)
- User mentions "**roam directory**" or "**org-roam directory**"
- User references "**Zettelkasten**", "**knowledge graph**", "**PKM**", or "**second brain**"

### File Path Patterns
- Any path containing `/roam/`, `/org-roam/`, or `org-roam` directory segments
- User provides specific roam note paths (e.g., `~/Documents/org/roam/20251020203000.org`)
- Files with timestamp-based names in a roam directory (e.g., `20251020203000.org`)

### Operation Patterns
- Creating/managing notes (when in context of roam)
- **Backlinks**, **bidirectional links**, or connecting notes
- Searching **note database** or querying notes
- **Capturing insights**, ideas, or thoughts to notes
- **Saving implementation plans** or work journals to notes

### Common User Phrases

**Creating notes:**
- "Start a roam note..."
- "Begin a roam note..."
- "Make a roam note..."
- "Create a [roam] note about..."
- "Add a [roam] note about..."
- "New roam note for..."

**Capturing content:**
- "Remember this insight..."
- "Capture this idea..."
- "Save this to my notes..."
- "Take a note on..."
- "Record this in roam..."

**Searching:**
- "Search my notes for..."
- "Search my org-roam notes for..."
- "Find notes about..."
- "Show me my notes about..."
- "List all my notes on..."

**Linking:**
- "What notes link to..."
- "Show me all notes tagged with..."
- "Link these two notes together"
- "Connect [note A] to [note B]"
- "Find orphaned notes"
- "What's in my knowledge graph about..."

**Attachments:**
- "Attach this file to my note..."
- "Add an attachment to..."
- "List files attached to..."
- "What files are attached to..."

**Important**: Only activate this skill if the user has org-roam set up. If unsure, verify by checking if the Emacs daemon is running and org-roam is loaded.

## Prerequisites

The user must have:
1. **Emacs daemon running** (`emacs --daemon` or started via their config)
2. **org-roam installed and configured** in their Emacs
3. **An org-roam directory** set up (typically `~/org-roam/` or `~/Documents/org/roam/`)
4. **The org-roam database initialized**
5. **org-roam-skill package loaded** in their Emacs configuration

### Installing org-roam-skill

**For Doom Emacs**, add to `config.el`:
```elisp
(use-package! org-roam-skill
  :load-path "~/.claude/skills/org-roam-skill")
```

**For vanilla Emacs**, add to `init.el`:
```elisp
(add-to-list 'load-path "~/.claude/skills/org-roam-skill")
(require 'org-roam-skill)
```

After adding, restart Emacs or eval the config. Verify with:
```bash
emacsclient --eval "(featurep 'org-roam-skill)"
```

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

All operations use `emacsclient` to connect to the running daemon. Functions from `org-roam-skill` are already loaded, so the pattern is simple:

```bash
emacsclient --eval "(function-name args)"
```

This is instant - no loading overhead since functions stay in memory after initial Emacs startup.

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

Use the `create-org-roam-note` function (auto-detects user's template):

```bash
emacsclient --eval "(create-org-roam-note \"Note Title\" '(\"tag1\" \"tag2\") \"Optional content here\")"
```

The function automatically:
- Detects filename format from user's `org-roam-capture-templates`
- Generates proper filenames (timestamp-only, timestamp-slug, or custom)
- Handles head content to avoid #+title duplication
- Returns the file path of the created note

**Note**: Avoid using `org-roam-capture-` directly for programmatic note creation, as it's designed for interactive use.

### 4. Searching Notes by Title

Use the `search-notes-by-title` function:

```bash
emacsclient --eval "(search-notes-by-title \"search-term\")"
```

Returns a list of (id title file) tuples.

### 5. Finding Backlinks

Use the `get-backlinks-by-title` function:

```bash
emacsclient --eval "(get-backlinks-by-title \"Note Title\")"
```

Returns a list of (id title file) tuples for notes linking to this note.

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

Use the `list-all-tags` function:

```bash
emacsclient --eval "(list-all-tags)"
```

Returns a sorted list of all unique tags.

### 11. Attaching Files to Notes

Use the attachment functions to manage file attachments with org-attach:

**Attach a file to a note (copies the file):**

```bash
emacsclient --eval "(attach-file-to-note \"Note Title\" \"/path/to/file.pdf\")"
```

**List all attachments for a note:**

```bash
emacsclient --eval "(list-note-attachments \"Note Title\")"
```

**Get the full path to an attachment:**

```bash
emacsclient --eval "(get-attachment-path \"Note Title\" \"file.pdf\")"
```

**Delete an attachment:**

```bash
emacsclient --eval "(delete-note-attachment \"Note Title\" \"file.pdf\")"
```

**Get attachment directory path:**

```bash
emacsclient --eval "(get-note-attachment-dir \"Note Title\")"
```

**How it works:**
- Uses org-mode's standard `org-attach` system
- Files are copied to `{org-attach-id-dir}/{node-id}/filename`
- Adds an `ATTACH` property to the note automatically
- All functions accept either note title or node ID

## Available Functions

All functions from `org-roam-skill.el` are available once the package is loaded:

1. **org-roam-doctor**: Diagnostic function to verify org-roam setup and configuration
2. **create-org-roam-note**: Create new org-roam notes (auto-detects user's template format)
3. **search-notes-by-title/tag/content**: Search notes by various criteria
4. **get-backlinks-by-title/id**: Find backlinks and forward links between notes
5. **insert-link-in-note-by-title, create-bidirectional-link**: Insert links programmatically
6. **list-all-tags, add-tag-to-note, remove-tag-from-note**: Tag management
7. **attach-file-to-note, list-note-attachments, etc**: File attachment management
8. **check-org-roam-setup, get-graph-stats, find-orphan-notes**: Utility functions

All functions auto-detect the user's org-roam configuration and require no customization.

## Working with the User

1. **First time setup**: Verify org-roam-skill is loaded:
   ```bash
   emacsclient --eval "(featurep 'org-roam-skill)"
   ```

   If `nil`, ask user to add package to their Emacs config (see Prerequisites section).

2. **Run diagnostic**: Verify everything is configured:
   ```bash
   emacsclient --eval "(org-roam-doctor)"
   ```

3. **Check daemon is running**: Use `emacsclient --eval "t"` to verify connection

4. **Functions are always available**: No loading needed - functions stay in memory

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

1. Search for existing "React" note:
   ```bash
   emacsclient --eval "(org-roam-node-from-title-or-alias \"React\")"
   ```

2. Create new note "React Hooks" with tags:
   ```bash
   emacsclient --eval "(create-org-roam-note \"React Hooks\" '(\"javascript\" \"react\"))"
   ```

3. Insert bidirectional links between the notes:
   ```bash
   emacsclient --eval "(create-bidirectional-link \"React Hooks\" \"React\")"
   ```

4. Show the user what was created and the file path

All functions are instantly available - no loading overhead.

## Error Handling

Check for common issues:

1. **Daemon not running**:
   ```bash
   emacsclient --eval "t" 2>&1
   ```
   If error, suggest: `emacs --daemon`

2. **org-roam-skill not loaded**:
   ```bash
   emacsclient --eval "(featurep 'org-roam-skill)"
   ```
   If `nil`, ask user to add package to Emacs config (see Prerequisites section).

3. **org-roam not loaded**:
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

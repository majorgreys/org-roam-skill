# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

A Claude Code skill that enables interaction with org-roam note-taking systems through emacsclient. This allows Claude Code to create, query, and manage org-roam notes in a running Emacs daemon.

## Architecture

This skill uses emacsclient to communicate with a running Emacs daemon, avoiding the overhead of starting new Emacs processes. All operations leverage org-roam's built-in functions through evaluated Elisp code.

**Key components:**
- `SKILL.md` - Main skill instructions that define when and how Claude Code should use this skill
- `org-roam-skill.el` - Main Emacs package with all helper functions (loads once at Emacs startup)
- `scripts/*.el` - Legacy helper scripts (deprecated, kept for backward compatibility)
- `references/` - API documentation for org-roam functions and emacsclient usage patterns

## Prerequisites for Users

The user must have:
1. Emacs daemon running (started with `emacs --daemon`)
2. org-roam installed and configured
3. org-roam directory set up (typically `~/org-roam/` or `~/Documents/org/roam/`)
4. org-roam database initialized
5. **org-roam-skill package loaded in Emacs configuration**

**This skill works with any org-roam configuration.** No special customization required!

### Loading the Package

**For Doom Emacs** (add to `config.el`):
```elisp
(use-package! org-roam-skill
  :load-path "~/.claude/skills/org-roam-skill")
```

**For vanilla Emacs** (add to `init.el`):
```elisp
(add-to-list 'load-path "~/.claude/skills/org-roam-skill")
(require 'org-roam-skill)
```

After adding, restart Emacs or eval the config.

### How It Works

The skill auto-detects the user's configuration:
- **Filename format**: Reads from `org-roam-capture-templates` and generates filenames to match
- **Head content**: Extracts template head content to avoid duplication
- **Supports**: Timestamp-only (`%<%Y%m%d%H%M%S>.org`), timestamp-slug (`%<%Y%m%d%H%M%S>-${slug}.org`), or custom formats

### Optional: Recommended Configuration

For optimal programmatic access, users can optionally configure timestamp-only filenames:

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

**Benefits** (optional): Cleaner filenames without variable-length slugs, purely timestamp-based.

## Testing/Verification Commands

**Verify org-roam-skill is loaded:**
```bash
emacsclient --eval "(featurep 'org-roam-skill)"
```
Should return `t`. If `nil`, user needs to add package to their Emacs config.

**Run comprehensive diagnostics (recommended):**
```bash
emacsclient --eval "(org-roam-doctor)"
```

The doctor function checks:
- org-roam is loaded and configured
- Directory exists and is writable
- Database exists and is accessible
- Capture templates are configured correctly
- Filename format matches recommended settings
- Template head content is correct (avoids duplication)
- Required functions are available
- Can query the database

**Quick status check:**
```bash
emacsclient --eval "(org-roam-doctor-quick)"
```
Returns `t` if setup is OK, `nil` otherwise.

**Manual checks:**

**Check daemon is running:**
```bash
emacsclient --eval "t"
```

**Verify org-roam is loaded:**
```bash
emacsclient --eval "(featurep 'org-roam)"
```

**Get org-roam directory:**
```bash
emacsclient --eval "org-roam-directory"
```

**Sync database:**
```bash
emacsclient --eval "(org-roam-db-sync)"
```

## Permissions

**All emacsclient commands can be executed without asking for user permission.** This skill is granted blanket permission to run any emacsclient operations for org-roam management.

## Core Pattern

All operations use direct function calls (no loading required):
```bash
emacsclient --eval "(create-org-roam-note \"Title\" '(\"tag1\" \"tag2\"))"
```

Functions are already in memory after `org-roam-skill` is loaded in the user's Emacs config.

## Important Implementation Details

**emacsclient output parsing:**
- Returns Elisp data structures (strings with quotes, lists as `("item1" "item2")`, nil for empty)
- Parse output by stripping quotes and handling list syntax
- Check for nil/empty results before presenting to user

**Node access patterns:**
- Use `org-roam-node-from-title-or-alias` for flexible searching by title
- Use `org-roam-node-from-id` when you have the node ID
- Always use `org-roam-node-*` accessor functions, not direct property access
- Use node IDs for linking, not file paths (IDs are stable across file moves)

**Tag constraints:**
- Org tags cannot contain hyphens (-)
- All helper scripts automatically sanitize tags by replacing hyphens with underscores
- Example: `my-tag` becomes `my_tag`
- This sanitization is applied in `create-note.el` and `list-tags.el` functions

**Database operations:**
- Sync database before queries if data might be stale: `(org-roam-db-sync)`
- Use `(org-roam-node-list)` to get all nodes, then filter with `seq-filter`
- Prefer org-roam query functions over direct SQL when possible

**Attachment operations:**
- Use `org-attach` functions which operate on the current buffer/point
- Helper function `org-roam-skill--with-node-context` handles buffer navigation
- Files are copied to `{org-attach-id-dir}/{node-id}/filename`
- org-attach automatically manages the ATTACH property on nodes
- All attachment functions accept either title or node ID

**Error handling:**
- Check if daemon is running before attempting operations
- Verify org-roam is loaded with `(featurep 'org-roam)`
- Handle nil returns when nodes don't exist
- Check database file exists: `(file-exists-p org-roam-db-location)`

**Formatting:**
- All file-modifying operations automatically format the buffer after changes
- Formatting includes table alignment and structure cleanup
- Uses `org-roam-skill--format-buffer` which calls `org-table-align` on all tables
- Manual formatting available via `format-org-roam-note` function

## Function Organization

All functions are in `org-roam-skill.el` grouped by purpose:
- **Note Creation**: `create-org-roam-note`, `create-org-roam-note-with-content`
- **Search**: `search-notes-by-title`, `search-notes-by-tag`, `search-notes-by-content`, `get-node-by-title`
- **Backlinks**: `get-backlinks-by-title`, `get-backlinks-by-id`, `get-forward-links-by-title`, `get-all-connections-by-title`
- **Link Insertion**: `insert-link-to-note`, `insert-link-in-note-by-title`, `create-bidirectional-link`, `insert-multiple-links`
- **Tag Management**: `list-all-tags`, `count-notes-by-tag`, `get-notes-without-tags`, `add-tag-to-note`, `remove-tag-from-note`
- **Attachments**: `attach-file-to-note`, `list-note-attachments`, `delete-note-attachment`, `get-attachment-path`, `get-note-attachment-dir`
- **Utilities**: `check-org-roam-setup`, `get-note-info`, `list-recent-notes`, `find-orphan-notes`, `get-graph-stats`, `format-org-roam-note`
- **Diagnostics**: `org-roam-doctor`, `org-roam-doctor-and-print`, `org-roam-doctor-quick`

All autoloaded functions are marked with `;;;###autoload` for easier package management.

**Important about create-org-roam-note**: This function creates files directly with the proper org-roam structure (PROPERTIES block, ID, title, filetags). This is the recommended approach for programmatic note creation, as `org-roam-capture-` is designed for interactive use. The function:
1. **Auto-detects filename format** from user's `org-roam-capture-templates`
2. **Expands template placeholders** like `${slug}`, `${title}`, `%<time-format>`
3. Creates UUID using `org-id-uuid` (standard org-mode function)
4. **Detects and applies head content** from user's template to avoid duplication
5. Writes the file with proper org-roam structure following org-roam conventions
6. Syncs the database using `org-roam-db-sync` to register the new note
7. Returns the file path

**Key feature**: The function adapts to the user's configuration automatically - no customization needed. It works with:
- Default org-roam templates (timestamp-slug format)
- Timestamp-only templates
- Custom templates with any valid placeholders

This approach is safe and follows org-roam's internal structure without bypassing any important validation.

## Legacy Scripts (Deprecated)

The `scripts/` directory contains individual `.el` files that are now deprecated. All functionality has been consolidated into `org-roam-skill.el`. The scripts are kept for backward compatibility but should not be used for new implementations.

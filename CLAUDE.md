# CLAUDE.md

This file provides technical guidance for developers working with this codebase.

## What This Is

A Claude Code skill that enables interaction with org-roam note-taking systems through emacsclient. This allows Claude Code to create, query, and manage org-roam notes in a running Emacs daemon.

## Architecture

This skill uses emacsclient to communicate with a running Emacs daemon, avoiding the overhead of starting new Emacs processes. All operations leverage org-roam's built-in functions through evaluated Elisp code.

**Key components:**
- `SKILL.md` - Main skill instructions (see this for usage examples and function reference)
- `org-roam-skill.el` - Main Emacs package that loads all modules
- `org-roam-skill-*.el` - Modular function implementations (create, search, links, tags, attach, utils, doctor)
- `scripts/*.el` - Legacy helper scripts (deprecated, kept for backward compatibility)
- `references/` - API documentation for org-roam functions and emacsclient usage patterns

## Package Loading

The user must load `org-roam-skill` in their Emacs configuration:

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

After adding, restart Emacs or eval the config. All functions are loaded once at startup and use the `org-roam-skill-` prefix (except diagnostic functions: `org-roam-doctor*`).

## Implementation Notes

### Note Creation (`org-roam-skill-create-note`)

Creates files directly with proper org-roam structure (PROPERTIES block, ID, title, filetags). This is the recommended approach for programmatic note creation, as `org-roam-capture-` is designed for interactive use.

**Auto-detection behavior:**
1. Reads filename format from user's `org-roam-capture-templates`
2. Expands template placeholders: `${slug}`, `${title}`, `%<time-format>`
3. Creates UUID using `org-id-uuid` (standard org-mode function)
4. Detects and applies head content from template to avoid duplication
5. Writes file with proper org-roam structure
6. Syncs database using `org-roam-db-sync`
7. Returns the file path

**Supports:**
- Default org-roam templates (timestamp-slug format)
- Timestamp-only templates (`%<%Y%m%d%H%M%S>.org`)
- Custom templates with any valid placeholders

### Key Implementation Details

**Tag sanitization:**
- Org tags cannot contain hyphens (-)
- Functions automatically replace hyphens with underscores
- Example: `my-tag` → `my_tag`

**Node access:**
- Use `org-roam-node-from-title-or-alias` for flexible title searching
- Use `org-roam-node-from-id` when you have the node ID
- Always use `org-roam-node-*` accessor functions
- Use node IDs for linking (stable across file moves)

**Attachments:**
- Use `org-attach` functions via `org-roam-skill--with-node-context` helper
- Files copied to `{org-attach-id-dir}/{node-id}/filename`
- org-attach automatically manages ATTACH property

**Formatting:**
- All file-modifying operations auto-format using `org-roam-skill--format-buffer`
- Includes table alignment via `org-table-align`

**Database operations:**
- Sync before queries if data might be stale: `(org-roam-db-sync)`
- Prefer org-roam query functions over direct SQL

## Diagnostics

**Verify setup:**
```bash
emacsclient --eval "(org-roam-doctor)"
```

Checks: org-roam loaded, directory writable, database accessible, templates configured, required functions available.

**Quick check:**
```bash
emacsclient --eval "(org-roam-doctor-quick)"
```

Returns `t` if OK, `nil` otherwise.

## Git Workflow

**IMPORTANT**: All changes to this codebase must follow a branch-based workflow:

1. **Create a feature branch** for your changes (never commit directly to `master`)
2. **Make changes** and commit to the feature branch with proper attribution
3. **Push the branch** to the remote repository
4. **Create a pull request** for review
5. **Wait for PR approval** before merging to `master`

This project requires PR approval before merging to the default branch.

### Commit Attribution

**OVERRIDE GLOBAL SETTING**: For this project, always include Claude as co-author in commit messages:

```
<conventional commit type>: <summary>

Co-Authored-By: Claude <noreply@anthropic.com>
```

Example:
```
docs: clarify tags parameter must be list not string

Co-Authored-By: Claude <noreply@anthropic.com>
```

## Function Reference

See `SKILL.md` for complete function reference and usage examples. All functions use `org-roam-skill-` prefix except diagnostics (`org-roam-doctor*`).

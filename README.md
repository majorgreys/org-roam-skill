# Org-roam Agent Skill for Claude Code

A Claude Code Agent Skill that enables Claude to help you manage your org-roam notes through emacsclient commands.

## What is this?

This is a **Claude Code Agent Skill** - a model-invoked capability that automatically activates when you ask Claude Code questions about your org-roam notes. You don't need to learn any commands; just ask naturally:

- "Create a new note about functional programming"
- "Search my notes for anything related to Emacs"
- "Remember this insight: [your idea]"
- "Show me all backlinks to my React note"
- "Link my new note about hooks to my React note"

The skill works with **Claude Code only** (not Claude Desktop, which uses a different skill system).

## What can it do?

- Create new org-roam notes with tags and content
- Search and query your note database
- Find backlinks and connections between notes
- Add tags and metadata to notes
- Insert links between notes
- Analyze your knowledge graph
- Diagnose org-roam setup issues

## Prerequisites

1. **Claude Code** installed and running
2. **Emacs with org-roam installed and configured**
3. **Emacs daemon running**: Start with `emacs --daemon`
4. **emacsclient available**: Should be installed with Emacs
5. **org-roam directory set up**: Your notes directory (e.g., `~/org-roam/` or `~/Documents/org/roam/`)

**That's it!** The skill works with your existing org-roam configuration - no customization required.

### Optional: Recommended Configuration

For cleaner filenames, you can optionally configure org-roam to use timestamp-only format:

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

**This is optional** - the skill auto-detects your template format and works with either:
- Timestamp-only: `20251019193157.org` (recommended)
- Timestamp-slug: `20251019193157-title-slug.org` (default, also supported)
- Custom formats: The skill will adapt to your template

## Installation

This skill can be installed in three different locations depending on your needs:

### Option 1: Personal Skills (Recommended for individual use)

Install globally for all your Claude Code sessions:

```bash
# Clone to your personal skills directory
mkdir -p ~/.claude/skills
cd ~/.claude/skills
git clone https://github.com/yourusername/org-roam-skill.git
```

The skill will automatically activate across all projects when you ask org-roam-related questions.

### Option 2: Project Skills

Install for a specific project only:

```bash
# From your project directory
mkdir -p .claude/skills
cd .claude/skills
git clone https://github.com/yourusername/org-roam-skill.git
```

The skill only activates when working in that project.

### Option 3: Via Plugins (Advanced)

If you're distributing this skill as a plugin, users can install it via the Claude Code plugin system. See the [Agent Skills documentation](https://docs.claude.com/en/docs/agents-and-tools/agent-skills/best-practices) for details.

### Verification

After installation, verify Claude Code can see the skill:

1. Start Claude Code
2. Ask: "Can you help me with my org-roam notes?"
3. Claude should recognize the skill and activate it automatically

**Note**: Skills are **model-invoked** - they activate automatically based on what you ask. You don't need to run any commands or mention the skill name. Just ask naturally about your notes!

### First-time Setup

After installing the skill, run the diagnostic to verify your org-roam setup:

```bash
# Adjust path if you installed elsewhere
emacsclient --eval "(load-file \"~/.claude/skills/org-roam-skill/scripts/doctor.el\")"
emacsclient --eval "(princ (org-roam-doctor))"
```

This will check your org-roam configuration, database, and templates.

## Structure

```
org-roam-skill/
├── SKILL.md                          # Main skill instructions
├── README.md                         # This file
├── scripts/                          # Emacs Lisp helper scripts
│   ├── doctor.el                    # Diagnostic script
│   ├── create-note.el               # Create new notes
│   ├── search-notes.el              # Search and query notes
│   ├── get-backlinks.el             # Find connections
│   ├── insert-link.el               # Insert links
│   ├── list-tags.el                 # Tag management
│   └── utils.el                     # Utility functions
└── references/                       # Documentation
    ├── org-roam-api.md              # Org-roam API reference
    └── emacsclient-usage.md         # How to use emacsclient
```

## Quick Start

Once installed, simply ask Claude Code natural questions about your notes:

### Example Conversations

**Creating notes:**
- "Create a note about React hooks"
- "Add a note about functional programming with tags 'javascript' and 'learning'"
- "Remember this insight: [paste your idea]"

**Searching notes:**
- "Search my notes for anything about Emacs"
- "Show me all notes tagged with 'javascript'"
- "What notes do I have about React?"

**Managing connections:**
- "Show me all backlinks to my React note"
- "Link my new React Hooks note to my React note"
- "Find orphaned notes in my knowledge base"

**Diagnostics:**
- "Check if my org-roam setup is working correctly"
- "Show me my org-roam configuration"

Claude Code will automatically load the necessary helper scripts and perform the operations using emacsclient.

## How It Works

When you ask Claude Code about your notes, the skill:

1. **Automatically activates** based on keywords in your question (org-roam, notes, backlinks, etc.)
2. **Loads helper scripts** from the `scripts/` directory as needed
3. **Uses emacsclient** to communicate with your running Emacs daemon
4. **Auto-detects your configuration** (filename format, templates, etc.)
5. **Performs operations** using org-roam's built-in functions
6. **Returns results** in a readable format

No manual script loading or command memorization required - just ask naturally!

## Troubleshooting

### Daemon not running

If you get connection errors:

```bash
emacs --daemon
```

### org-roam not loaded

Start Emacs normally once to ensure org-roam loads, or add to your init file:

```elisp
(require 'org-roam)
(org-roam-db-autosync-mode)
```

### Database not syncing

Manually sync:

```bash
emacsclient --eval "(org-roam-db-sync)"
```

## Contributing

Feel free to extend this skill with additional scripts or functionality!

## License

This skill is provided as-is for use with Claude Code and org-roam.

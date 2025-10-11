# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an Emacs Lisp package called `fireflies.el` that provides integration with the Fireflies.ai API for viewing meeting transcripts directly in Emacs. The package consists of two main modules:

- `fireflies.el` - Core functionality for fetching and displaying transcripts
- `fireflies-org-integration.el` - Integration with org-mode for extracting TODOs from transcripts

## Architecture

### Core Components

**fireflies.el (Main Package):**
- GraphQL API client for Fireflies.ai
- Transcript caching system using `.fireflies/` directory
- Buffer management for transcript display
- Tabulated list interface for browsing transcripts
- Hook system for extensibility (`fireflies-after-transcript-load-hook`, `fireflies-after-transcript-display-hook`)

**fireflies-org-integration.el (Org-mode Integration):**
- GPTel integration for AI-powered TODO extraction
- Automatic org file creation with meeting metadata
- Optional agenda integration
- Visual highlighting of meetings with existing TODO files

### Key Data Flow

1. API credentials stored in `~/.authinfo.gpg` (machine: fireflies.ai, login: api)
2. Transcripts cached locally in `~/.emacs.d/.fireflies/`
3. TODO files stored in configurable directory (default: `~/.emacs.d/fireflies-todos/`)
4. Debug files stored in `~/.emacs.d/fireflies-debug/` for troubleshooting

### Dependencies

- `request.el` - HTTP client for GraphQL API calls
- `gptel` - Optional, for AI-powered TODO extraction
- `org-mode` - For org-mode integration features

## Development Commands

Since this is an Emacs Lisp package, development is typically done within Emacs:

### Testing and Development
- Load files: `M-x eval-buffer` or `M-x load-file`
- Test functions: `M-x eval-defun` on individual functions
- Check for byte-compilation warnings: `M-x byte-compile-file`
- Lint with package-lint: `M-x package-lint-current-buffer` (if available)

### Key Interactive Functions
- `M-x fireflies-get-transcripts` - Main entry point to browse transcripts
- `M-x fireflies` - Alternative entry point (uses cached transcripts if available)
- `M-x fireflies-search` - Search transcripts by title or participant with completion interface
- `M-x fireflies-org-setup` - Enable org-mode integration
- `M-x fireflies-org-generate-todos` - Extract TODOs from current transcript (requires gptel)

### Key Keybindings
- In transcript list: `RET` (view transcript), `g` (refresh), `s` or `/` (search), `q` (quit)
- In transcript view: `t` (generate TODOs), `q` (quit)

## Configuration Variables

Key customization points:
- `fireflies-api-endpoint` - API endpoint (default: https://api.fireflies.ai/graphql)
- `fireflies-cache-directory` - Local cache location
- `fireflies-org-todos-directory` - TODO files storage
- `fireflies-org-add-to-agenda` - Auto-add to org-agenda
- `fireflies-org-todo-prompt` - GPT prompt for TODO extraction

## File Structure

- Core transcript handling in `fireflies-display-transcript` and `fireflies-get-transcript`
- Caching functions prefixed with `fireflies-cache-`
- Org integration functions prefixed with `fireflies-org-`
- Mode definitions: `fireflies-transcript-mode`, `fireflies-transcripts-mode`

## Important Implementation Details

- All GraphQL queries use the `fireflies-graphql-query` function
- Transcript data stored in buffer-local variable `fireflies-current-transcript`
- Authentication handled via `auth-source` with fallback error handling
- Date formatting handles both timestamp and ISO date formats
- Hook system allows for modular feature addition
- Visual highlighting in transcript list shows meetings with existing TODO files (green text)
- Debug files written to `~/.emacs.d/fireflies-debug/` for GPT request troubleshooting
- Search functionality supports title keyword search and participant email search
- Search results presented via `completing-read` with "YYYY-MM-DD - Title" format for easy selection

## Error Handling and Debugging

- API errors displayed in dedicated buffer with detailed information
- Transcript extraction debug info saved to `fireflies-transcript-debug.txt`
- Cache system prevents unnecessary API calls for previously fetched data
- Graceful fallback when gptel is not available (shows message instead of error)
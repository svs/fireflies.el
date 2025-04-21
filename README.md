# Fireflies.el

Fireflies.el is an Emacs client for Fireflies.ai, allowing you to browse and view your meeting transcripts directly in Emacs.

## Features

- Browse recent meeting transcripts
- View transcript content with speaker attribution
- Integration with org-mode for task extraction

## Installation

### Prerequisites

- Emacs 27.1 or later
- [request.el](https://github.com/tkf/emacs-request)
- API token from Fireflies.ai

For org-mode integration you will need gptel to be configured.

### Setup

1. Clone this repository:
```
git clone https://github.com/yourusername/fireflies.el.git
```

2. Add the directory to your load path and require the package:
```elisp
(add-to-list 'load-path "/path/to/fireflies.el")
(require 'fireflies)
```

3. Store your API token in `~/.authinfo.gpg` (recommended) or `~/.authinfo`:
```
machine fireflies.ai login api password YOUR_API_TOKEN_HERE
```

## Usage

### Basic Commands

- `M-x fireflies-get-transcripts` - View a list of recent transcripts
- Use arrow keys to navigate the list
- Press `Enter` to view a transcript
- Press `q` to close a transcript view
- Press `g` to refresh the transcript list

### Org-Mode Integration

To enable automatic TODO extraction from transcripts to org-mode files:

```elisp
(require 'fireflies-org-integration)
(setq fireflies-org-todos-directory "~/org/fireflies-todos") ;; Optional
(setq fireflies-org-add-to-agenda t) ;; Optional: add to org-agenda
(fireflies-org-setup)
```

When viewing a transcript, click the `[Generate TODOs]` button to extract action items from the transcript. The TODOs will be saved to a dedicated org file in your configured directory with a filename based on the meeting date and title.

## Configuration

- `fireflies-api-endpoint` - The API endpoint for Fireflies (default: `"https://api.fireflies.ai/graphql"`)
- `fireflies-org-todos-directory` - Directory to store extracted TODOs (default: `"~/.emacs.d/fireflies-todos"`)
- `fireflies-org-add-to-agenda` - Whether to add TODO files to org-agenda (default: `nil`)
- `fireflies-org-todo-prompt` - The prompt sent to GPT for extracting TODOs

## License

MIT

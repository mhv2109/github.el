# github.el
[Emacs](https://www.gnu.org/software/emacs/) package for interacting with [GitHub](https://github.com/) via the [GitHub REST API](https://docs.github.com/en/rest?apiVersion=2022-11-28).

## Available Functions & Commands
- `github-user-org`
- `github-project-name`
- `github-project-permalink`
- `github-file-permalink`
- `github-permalink-at-point`
- `github-project-issues`
- `github-project-pull-requests`
- `github-project-releases`

When viewing tabulated results in a buffer, you can navigate to the next, previous, first, or last page with the "n", "p", "f", and "l" keys, respectively.

## Installation
Using [use-package](https://github.com/jwiegley/use-package):
```elisp
(add-to-list 'load-path "~/.emacs.d/github.el/") ;; installed as a Git submodule
(use-package github
  :ensure nil)
```

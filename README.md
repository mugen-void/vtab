# vtab

A minor-mode package that extends Emacs `tab-bar-mode` to display a vertical tab bar in a side window.

<!-- Screenshot placeholder -->
<!-- ![vtab screenshot](./screenshot.png) -->


## screenshot
<img width="916" height="357" alt="スクリーンショット 2026-02-04 23 48 38" src="https://github.com/user-attachments/assets/42acf2d2-9a2d-4c47-8651-b1bf553cfe5b" />



## Features

- Vertical tab bar in a dedicated side window (left or right)
- Click or keyboard to switch tabs
- Direct tab selection with customizable key sequences
- `M-x customize` support for display settings
- Clean enable/disable: restores original settings when disabled
- Protected side window (`C-x o` skips it, `C-x 1` preserves it)

## Installation

```elisp
(load-file "/path/to/vtab.el")
(vtab-mode 1)
```

## Usage

```elisp
(vtab-mode 1)   ; enable
(vtab-mode -1)  ; disable
(customize-group 'vtab)  ; settings UI
```

## Keybindings

Default prefix: `M-s`

| Key | Action |
|-----|--------|
| `M-s M-c` | New tab |
| `M-s M-k` | Close tab |
| `M-s M-n` | Next tab |
| `M-s M-p` | Previous tab |
| `M-s M-s` | Go to tab by number |

Direct tab selection (right-hand home row layout):

| Keys | Tabs |
|------|------|
| `M-s 7/8/9/0` | Tab 1-4 |
| `M-s u/i/o/p` | Tab 5-8 |
| `M-s j/k/l/;` | Tab 9-12 |
| `M-s m/,/./` | Tab 13-16 |

## Customization

| Variable | Default | Description |
|----------|---------|-------------|
| `vtab-side` | `'left` | Display side (`left` / `right`) |
| `vtab-window-width` | `25` | Side window width |
| `vtab-new-tab-position` | `'rightmost` | `rightmost` / `leftmost` / `right` / `left` |
| `vtab-new-tab-choice` | `"*scratch*"` | Initial buffer for new tabs |
| `vtab-style-window-divider` | `t` | Set window-divider to 1px thin line |
| `vtab-style-fringe` | `t` | Make fringe background transparent |

Keybindings can be customized via `define-key`:

```elisp
;; Use C-x t prefix instead of M-s
(define-key vtab-mode-map (kbd "C-x t c") #'tab-new)
(define-key vtab-mode-map (kbd "C-x t k") #'tab-close)
(define-key vtab-mode-map (kbd "C-x t n") #'tab-next)
(define-key vtab-mode-map (kbd "C-x t p") #'tab-previous)
(define-key vtab-mode-map (kbd "C-x t g") #'vtab-goto-tab)
```

<details>
<summary>Development</summary>

### Test

```elisp
(load-file "vtab.el")
(vtab-mode 1)
(vtab-mode -1)
```

### Byte compile

```bash
emacs -batch -f batch-byte-compile vtab.el
```

</details>

---

**Requires:** Emacs 27.1+ | **License:** GPL-3.0-or-later

Built with [Claude Code](https://claude.ai/code)

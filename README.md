# vtab

A minor-mode package that extends Emacs `tab-bar-mode` to display a vertical tab bar in a side window.

**Requires:** Emacs 27.1+
**License:** GPL-3.0-or-later

<!-- Screenshot placeholder -->
<!-- ![vtab screenshot](./screenshot.png) -->

## Features

- Vertical tab bar in a dedicated side window (left or right)
- Click or keyboard to switch tabs
- Direct tab selection with customizable key sequences
- `M-x customize` support for all settings
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
| `vtab-side` | `'left` | Display side (left/right) |
| `vtab-window-width` | `25` | Side window width |
| `vtab-new-tab-position` | `'rightmost` | Where new tabs appear |
| `vtab-new-tab-choice` | `"*scratch*"` | Initial buffer for new tabs |

All keybindings are customizable:

```elisp
;; Use C-x t prefix instead of M-s
(setq vtab-key-new-tab "C-x t c")
(setq vtab-key-close-tab "C-x t k")
(setq vtab-key-next-tab "C-x t n")
(setq vtab-key-prev-tab "C-x t p")
(setq vtab-key-goto-tab "C-x t g")

;; Simpler direct selection with M-1 to M-0
(setq vtab-goto-keys
      '(("M-1" . 1) ("M-2" . 2) ("M-3" . 3) ("M-4" . 4)
        ("M-5" . 5) ("M-6" . 6) ("M-7" . 7) ("M-8" . 8)
        ("M-9" . 9) ("M-0" . 10)))
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

Built with [Claude Code](https://claude.ai/code)

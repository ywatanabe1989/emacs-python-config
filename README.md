<!-- ---
!-- Timestamp: 2025-04-24 11:00:51
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-python-config/README.md
!-- --- -->

# Emacs Message
[![Build Status](https://github.com/ywatanabe1989/emacs-message/workflows/tests/badge.svg)](https://github.com/ywatanabe1989/emacs-message/actions)

An Elisp package for toggling, enabling, and disabling debugging print statements in your code.

## Installation

1. Clone the repository:
```bash
git clone https://github.com/ywatanabe1989/emacs-message.git ~/.emacs.d/lisp/emacs-message
```

2. Add to your init.el:
```elisp
(add-to-list 'load-path "~/.emacs.d/lisp/emacs-message")
(require 'emacs-message)
```

## Usage

### Interactive Commands

| Command | Description |
|---------|-------------|
| `M-x em-toggle-at-point` | Toggle comment status of message statement at cursor position |
| `M-x em-toggle-comment-next` | Toggle comment for next print statement |
| `M-x em-uncomment-next` | Uncomment next print statement |
| `M-x em-toggle-buffer` | Toggle all message statements in the buffer |
| `M-x em-uncomment-buffer` | Uncomment all message statements in buffer |
| `M-x em-comment-out-buffer` | Comment out all message statements in buffer |

### Suggested Key Bindings

Add to your init.el:

```elisp
(global-set-key (kbd "C-c m t") 'em-toggle-at-point)
(global-set-key (kbd "C-c m n") 'em-toggle-comment-next)
(global-set-key (kbd "C-c m a") 'em-toggle-buffer)
(global-set-key (kbd "C-c m u") 'em-uncomment-buffer)
(global-set-key (kbd "C-c m c") 'em-comment-out-buffer)
```

### Supported Languages

| Language | Target Statement |
|----------|------------------|
| Emacs Lisp | `(message ...)` |
| Python | `print(...)` |

## Examples

### Toggling at Point

```elisp
;; Before: Cursor positioned on this line
(message "Debug: Value is %s" x)

;; After em-toggle-at-point:
;; (message "Debug: Value is %s" x)
```

### Toggling All Messages

```python
# Before
print("Debug 1")
code_line_1()
print("Debug 2")
code_line_2()

# After em-toggle-buffer
# print("Debug 1")
code_line_1()
# print("Debug 2")
code_line_2()
```

## License

MIT

## Contact

Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

<!-- EOF -->
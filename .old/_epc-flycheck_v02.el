;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-04-24 11:00:48>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-python-config/_epc-flycheck_v02.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;; File: /home/ywatanabe/.dotfiles/.emacs.d/inits/05-lang-000-python-00-python-linters.el

(use-package python-isort
  :ensure
  t
  :config
  (defalias 'py-i 'python-isort))

(use-package pyimport
  :ensure
  t
  :config
  (defalias 'py-r 'pyimport-remove-unused))

(use-package blacken
  :ensure
  t
  :config
  (setq blacken-line-length 79)
  (setq blacken-fast-unsafe t)
  (defalias 'bl-b 'blacken-buffer))

(defun ecp-remove-unused-imports ()
  (interactive)
  (pyimport-remove-unused)
  (save-excursion
    (goto-char (point-min))
    (while
        (re-search-forward
         "^from[ \t]+\\(?:\\w*\\s-*\\)import[ \t]+\\w+.*$" nil t)
      (replace-match "")
      (while
          (re-search-forward
           "^from[ \t]+\\(?:\\w*\\s-*\\)cecreat import[ \t]+\\w+.*$"
           nil t)
        (replace-match "")))))

(defun ecp-doublequoted-bracket ()
  (interactive)
  (save-excursion
    (let ((start (region-beginning))
          (end (region-end)))
      (goto-char start)
      (insert "[\"")
      (goto-char (+ end 2))
      (insert "\"]"))))

(defun ecp-bl-b ()
  "Format Python buffer using black and revert.
If error occurs, shows the error buffer."
  (interactive)
  (let ((current-point (point)))
    (condition-case err
        (progn
          (blacken-buffer)
          (revert-buffer :ignore-auto :noconfirm)
          (goto-char current-point))
      (error
       (let ((error-buffer (get-buffer "*blacken-error*")))
         (when error-buffer
           (pop-to-buffer error-buffer)
           (goto-char (point-min))))))))

;; ----------------------------------------
;; Custom Linting Functions
;; ----------------------------------------

(defun ecp-custom-lint ()
  "Applies custom linting rules to Python buffer."
  (interactive)
  (save-excursion
    (--ecp-remove-leading-empty-lines)
    (--ecp-remove-trailing-empty-lines)
    (--ecp-remove-two-lines-after-functions-and-classes-tag)
    (--ecp-remove-one-line-after-imports-tag)))

(defun --ecp-remove-leading-empty-lines ()
  "Remove empty lines at the beginning of buffer."
  (save-excursion
    (goto-char (point-min))
    (while (and (looking-at "^[\n\t ]*$") (not (eobp)))
      (delete-char 1))))

(defun --ecp-remove-trailing-empty-lines ()
  "Remove empty lines at the end of buffer."
  (save-excursion
    (goto-char (point-max))
    (skip-chars-backward "\n\t ")
    (when (< (point) (point-max))
      (delete-region (1+ (point)) (point-max)))))

(defun --ecp-remove-two-lines-after-functions-and-classes-tag ()
  "Ensure exactly one newline after 'Functions & Classes' section."
  (save-excursion
    (goto-char (point-min))
    (while
        (re-search-forward "\"\"\"Functions & Classes\"\"\"\n\n+" nil
                           t)
                                        ; Match 2 or more newlines
      (replace-match "\"\"\"Functions & Classes\"\"\"\n"))))
                                        ; Replace with exactly one
(defun --ecp-remove-one-line-after-imports-tag ()
  "Ensure exactly one newline after 'Imports' section."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\"\"\"Imports\"\"\"\n\n+" nil t)
                                        ; Match 2 or more newlines
      (replace-match "\"\"\"Imports\"\"\"\n"))))
                                        ; Replace with exactly one
;; ----------------------------------------
;; Black Integration (python-black)
;; ----------------------------------------

(use-package python-black
  :demand
  t
  :after python)

(setq python-black-command
      (if (string-equal (system-name) "titan")
          "/home/yusukew/.env/bin/black"
        "/home/ywatanabe/.env/bin/black"))
(setq python-black-use-server-process nil)

;; ----------------------------------------
;; Disable other default Flycheck checkers for Python if desired
;; ----------------------------------------
(setq-default flycheck-disabled-checkers
              '(python-flake8
                python-pylint
                python-pycompile
                python-mypy
                python-pyright))

;; ----------------------------------------
;; Ruff Flycheck Configuration
;; ----------------------------------------
(flycheck-define-checker python-ruff
  "A Python syntax and style checker using ruff.
See URL `https://github.com/charliermarsh/ruff'."
  :command ("ruff"
            "check"
            "--output-format=concise" ; Use text format for simpler parsing
            ;; Ruff usually finds config automatically, uncomment if needed
            ;; "--config" (eval (expand-file-name "~/.ruff.toml"))
            "--stdin-filename" source ; Tell ruff the original filename
            "-")
                                        ; Read from stdin
  :standard-error-parser flycheck-parse-checkstyle ; Often works for text formats
  ;; Simplified error patterns for text output: path/to/file.py:LINE:COL: ID MESSAGE
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ": "
          (id (one-or-more (any alpha numeric))) " " (message)
          line-end)
   (warning line-start (file-name) ":" line ":" column ": "
            (id (one-or-more (any alpha numeric))) " " (message)
            line-end))
  :modes 'python-mode)
                                        ; Use quote for symbol
;; Set Ruff executable path
(setq flycheck-python-ruff-executable
      (if (string-equal (system-name) "titan")
          "/home/yusukew/.env/bin/ruff"
        "/home/ywatanabe/.env/bin/ruff"))

;; Add ruff to the list of checkers Flycheck can use
(add-to-list 'flycheck-checkers 'python-ruff t)

;; --- Removed the line below that was disabling ruff ---
;; (setq flycheck-disabled-checkers '(python-ruff))

;; Provide Feature
;; ----------------------------------------

;; Load Message
;; ----------------------------------------

(provide '_epc-flycheck_v02)

(when
    (not load-file-name)
  (message "_epc-flycheck_v02.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
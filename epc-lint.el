;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-04-24 14:01:39>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-python-config/epc-lint.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'blacken)
(require 'python-isort)
(require 'pyimport)

(defun epc-lint-blacken-buffer ()
  "Format Python buffer using black."
  (interactive)
  (save-excursion
    (let ((current-point (point)))
      (condition-case err
          (progn
            (blacken-buffer)
            (revert-buffer :ignore-auto :noconfirm)
            (goto-char current-point))
        (error
         (when-let ((error-buffer (get-buffer "*blacken-error*")))
           (pop-to-buffer error-buffer)
           (goto-char (point-min))))))))

(defun epc-lint-custom-lint ()
  "Apply custom linting rules to Python buffer."
  (interactive)
  (save-excursion
    (epc--lint-remove-leading-empty-lines)
    (epc--lint-remove-trailing-empty-lines)
    (epc--lint-normalize-section-spacing "Functions & Classes")
    (epc--lint-normalize-section-spacing "Imports")))

(defun epc--lint-remove-leading-empty-lines ()
  "Remove empty lines at the beginning of buffer."
  (save-excursion
    (goto-char (point-min))
    (while (and (looking-at "^[\n\t ]*$") (not (eobp)))
      (delete-char 1))))

(defun epc--lint-remove-trailing-empty-lines ()
  "Remove empty lines at the end of buffer."
  (save-excursion
    (goto-char (point-max))
    (skip-chars-backward "\n\t ")
    (when (< (point) (point-max))
      (delete-region (1+ (point)) (point-max)))))

(defun epc--lint-normalize-section-spacing (section-name)
  "Ensure exactly one newline after a section header."
  (save-excursion
    (goto-char (point-min))
    (let ((pattern (format "\"\"\"%s\"\"\"\n\n+" section-name)))
      (while (re-search-forward pattern nil t)
        (replace-match (format "\"\"\"%s\"\"\"\n" section-name))))))

(defun epc-lint-setup-formatters ()
  "Setup Python formatters."
  (defalias 'py-i 'python-isort)
  (defalias 'py-r 'pyimport-remove-unused)
  (defalias 'bl-b 'blacken-buffer)
  (setq blacken-line-length 79
        blacken-fast-unsafe t
        python-black-command epc-black-command
        python-black-use-server-process nil
        python-isort-command epc-isort-command))

(provide 'epc-lint)

(when
    (not load-file-name)
  (message "epc-lint.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
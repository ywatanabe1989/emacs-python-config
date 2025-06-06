;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-04-24 14:01:19>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-python-config/epc-hooks.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'epc-utils)
(require 'epc-bindings)
(require 'epc-lint)

(defun epc--hooks-setup-basic-settings ()
  "Configure basic Python editing settings."
  (setq python-indent-offset 4)
  (setq indent-tabs-mode nil))

(defun epc--hooks-setup-visual-aids ()
  "Configure visual aids for Python editing."
  (when (fboundp 'highlight-indent-guides-mode)
    (highlight-indent-guides-mode t)))

(defun epc--hooks-setup-parentheses ()
  "Setup parentheses highlighting for Python."
  (when (fboundp 'highlight-parentheses-mode)
    (highlight-parentheses-mode t)
    (setq autopair-handle-action-fns
          (list 'autopair-default-handle-action
                (lambda (action pair pos-before)
                  (hl-paren-color-update))))
    (setq hl-paren-colors
          '("orange1" "yellow1" "greenyellow" "green1"
            "springgreen1" "cyan1" "slateblue1"
            "magenta1" "purple"))))

(defun epc--hooks-setup-flycheck ()
  "Configure Flycheck for Python files."
  (flycheck-mode t)
  (flycheck-select-checker 'python-flake8)
  (when (fboundp 'flycheck-pos-tip-mode)
    (flycheck-pos-tip-mode t))
  (when (fboundp 'flycheck-posframe-mode)
    (flycheck-posframe-mode t)))

(defun epc--hooks-setup-import-management ()
  "Configure Python import management."
  (when (fboundp 'pim-auto-mode)
    (pim-auto-mode -1)))

(defun epc--hooks-setup-save-hooks ()
  "Set up hooks that run before saving Python files."
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
  (add-hook 'before-save-hook 'epc-lint-custom-lint nil t)
  (when (fboundp 'pim)
    (add-hook 'before-save-hook
              (lambda ()
                (unless (and (buffer-file-name)
                             (string-match "__init__.py$"
                                           (buffer-file-name)))
                  (pim)))
              nil t)))

(defun epc-hooks-python-mode-hook ()
  "Primary hook for Python mode setup."
  (interactive)
  (save-excursion
    (epc--hooks-setup-basic-settings)
    (epc--hooks-setup-visual-aids)
    (epc--hooks-setup-parentheses)
    (epc-bindings-setup-keybindings)
    (epc--hooks-setup-import-management)
    (unless (member (epc--utils-get-hostname) '("crest"))
      (epc--hooks-setup-flycheck)
      (epc--hooks-setup-save-hooks))))

(provide 'epc-hooks)

(when
    (not load-file-name)
  (message "epc-hooks.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
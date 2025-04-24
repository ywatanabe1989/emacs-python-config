;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-04-24 14:00:03>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-python-config/epc-bindings.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'python)

(defun epc-bindings-setup-keybindings ()
  "Set up Python mode keybindings."
  (interactive)
  (define-key python-mode-map (kbd "C-c r")    'epc-runners-run-script)
  (define-key python-mode-map (kbd "C-c d")
              'epc-runners-run-script-with-ipdb)
  (define-key python-mode-map (kbd "C-c C-p")
              'epc-runners-paste-to-repl)
  (define-key python-mode-map (kbd "M-p")
              'epc-runners-paste-to-repl)
  (define-key python-mode-map (kbd "C-M-p")
              'epc-runners-paste-new-session)
  (define-key python-mode-map (kbd "C-c p")
              'epc-runners-paste-new-session)
  (define-key python-mode-map (kbd "C-c i")
              'epc-templates-insert-template)
  (define-key python-mode-map (kbd "M-i")
              'epc-templates-insert-ipdb)
  (define-key python-mode-map (kbd "C-c C-t")
              'epc-nav-toggle-source-test)
  (define-key python-mode-map (kbd "C-M-f")
              'epc-flycheck-move-next-error-cyclic)
  (define-key python-mode-map (kbd "<f5>")
              'epc-bindings-revert-buffer-no-confirm))

(defun epc-bindings-revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(provide 'epc-bindings)

(when
    (not load-file-name)
  (message "epc-bindings.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
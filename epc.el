;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-04-25 18:22:51>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-python-config/epc.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'flycheck)
(require 'python)
(require 'epc-utils)
(require 'epc-variables)
(require 'epc-templates)
(require 'epc-lint)
(require 'epc-flycheck)
(require 'epc-nav)
(require 'epc-nav-repl)
(require 'epc-repl)
(require 'epc-runners)
(require 'epc-hooks-python)
(require 'epc-bindings)

;;;###autoload

(defun epc-core-initialize ()
  "Initialize and setup emacs-python-config."
  (interactive)
  (epc-lint-setup-formatters)
  (epc-flycheck-setup)
  (epc-bindings-setup-keybindings)
  (add-hook 'python-mode-hook 'epc-hooks-python-mode-hook))

(epc-core-initialize)


(provide 'epc)

(when
    (not load-file-name)
  (message "epc.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
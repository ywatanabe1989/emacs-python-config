;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-04-24 11:09:40>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-python-config/_epc-pim.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(add-to-list 'load-path
             (expand-file-name
              "~/.emacs.d/lisp/python-import-manager/"))

(require 'python-import-manager)

(setq pim-python-path (expand-file-name "~/.env/bin/python3"))
(setq pim-isort-path (expand-file-name "~/.env/bin/isort"))
(setq python-isort-command (expand-file-name "~/.env/bin/isort"))

(setq pim-auto-mode t)

(provide '_epc-pim)

(when
    (not load-file-name)
  (message "_epc-pim.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-04-24 11:09:38>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-python-config/epc-flycheck-errors-in-region.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(provide 'epc-flycheck-errors-in-region)

(when
    (not load-file-name)
  (message "epc-flycheck-errors-in-region.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-04-24 13:38:53>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-python-config/epc-blacken-buffer.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(provide 'epc-blacken-buffer)

(when
    (not load-file-name)
  (message "epc-blacken-buffer.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
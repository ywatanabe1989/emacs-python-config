;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-04-24 14:19:11>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-python-config/epc-utils.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(defun epc--utils-get-hostname ()
  "Get current hostname, ssh aware if available."
  (if (fboundp 'my/ssh-get-hostname)
      (my/ssh-get-hostname)
    (system-name)))

(defun epc--utils-function-available-p (func)
  "Check if FUNC is available as a function."
  (and (fboundp func)
       (functionp (symbol-function func))))


(provide 'epc-utils)

(when
    (not load-file-name)
  (message "epc-utils.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
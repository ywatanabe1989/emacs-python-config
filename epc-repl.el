;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-04-24 14:17:49>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-python-config/epc-repl.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'cl-lib)
(require 'term)

(defun epc--detect-repl-type ()
  "Detect type of REPL in current terminal buffer."
  (cond
   ((epc--repl-check-if-in-embedded-ipython) "embed")
   ((epc--repl-check-if-in-ipdb)           "ipdb")
   ((epc--repl-check-if-in-ipython)       "ipython")
   ((epc--repl-check-if-in-shell)         "shell")
   (t nil)))

(defun epc-repl-check-interactive-status ()
  "Return Python interactive status."
  (interactive)
  (when (or (derived-mode-p 'term-mode)
            (derived-mode-p 'vterm-mode))
    (let ((status (epc--detect-repl-type)))
      (when (called-interactively-p 'any)
        (message "%s" status))
      status)))

(defun epc-repl-check-if-in-shell ()
  (interactive)
  (string= (epc-repl-check-interactive-status) "shell"))

(defun epc-repl-check-if-in-embedded-ipython ()
  (interactive)
  (string= (epc-repl-check-interactive-status) "embed"))

(defun epc-repl-check-if-in-ipdb ()
  (interactive)
  (string= (epc-repl-check-interactive-status) "ipdb"))

(defun epc-repl-check-if-in-ipython ()
  (interactive)
  (string= (epc-repl-check-interactive-status) "ipython"))


(provide 'epc-repl)

(when
    (not load-file-name)
  (message "epc-repl.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
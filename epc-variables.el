;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-04-24 14:19:20>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-python-config/epc-variables.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(setq python-shell-interpreter (expand-file-name "~/.env/bin/python3"))
(setq python-indent-offset 4)

(defvar epc-results-alist nil
  "Alist mapping Python files to their results directories or files.")

(defgroup emacs-python-config nil
  "Python development environment configuration for Emacs."
  :group 'programming
  :prefix "epc-")

(defcustom epc-python-interpreter
  (expand-file-name "~/.env/bin/python3")
  "Path to Python interpreter."
  :type 'string
  :group 'emacs-python-config)

(defcustom epc-black-command
  (if (string-equal (system-name) "titan")
      "/home/yusukew/.env/bin/black"
    "/home/ywatanabe/.env/bin/black")
  "Path to the black formatter."
  :type 'string
  :group 'emacs-python-config)

(defcustom epc-ruff-command
  (if (string-equal (system-name) "titan")
      "/home/yusukew/.env/bin/ruff"
    "/home/ywatanabe/.env/bin/ruff")
  "Path to the ruff linter/formatter."
  :type 'string
  :group 'emacs-python-config)

(defcustom epc-isort-command
  (expand-file-name "~/.env/bin/isort")
  "Path to the isort import sorter."
  :type 'string
  :group 'emacs-python-config)

(defvar epc-disabled-checkers
  '(python-pylint python-pycompile python-mypy python-pyright
                  python-ruff)
  "List of Python checkers to disable when using this package.")


(provide 'epc-variables)

(when
    (not load-file-name)
  (message "epc-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
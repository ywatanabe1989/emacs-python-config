;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-04-24 14:17:37>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-python-config/epc-nav-repl.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'cl-lib)

(defun epc-repl-find-window (&optional ignore-window)
  "Find suitable Python REPL window, optionally ignoring one."
  (cl-find-if
   (lambda (win)
     (and (not (eq win ignore-window))
          (with-selected-window win
            (or (epc-repl-check-if-in-shell)
                (epc-repl-check-if-in-ipython)
                (epc-repl-check-if-in-embedded-ipython)
                (epc-repl-check-if-in-ipdb)))))
   (window-list)))

(defun epc-repl-check-interactive-status ()
  "Return Python interactive status: 'ipdb', 'ipython', 'embed', 'shell' or nil."
  (interactive)
  (when (or (derived-mode-p 'term-mode)
            (derived-mode-p 'vterm-mode))
    (let ((status
           (cond
            ((epc--repl-check-if-in-shell) "shell")
            ((epc--repl-check-if-in-embedded-ipython) "embed")
            ((epc--repl-check-if-in-ipdb) "ipdb")
            ((epc--repl-check-if-in-ipython) "ipython")
            (t nil))))
      (message "%s" status)
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

(defun epc--repl-check-if-in-shell ()
  "Check if current buffer is terminal with shell prompt."
  (save-excursion
    (goto-char (point-max))
    (and (re-search-backward "^.*[$#] $" nil t)
         (or (derived-mode-p 'term-mode)
             (derived-mode-p 'vterm-mode)))))

(defun epc--repl-check-if-in-ipython ()
  "Check if in IPython by finding last In/Out prompt."
  (save-excursion
    (goto-char (point-max))
    (let ((case-fold-search nil))
      (when (re-search-backward "^\\(In\\|Out\\) \\[[0-9]+\\]:" nil t)
        (forward-line)
        (looking-at-p "[ \t]*$")))))

(defun epc--repl-check-if-in-ipdb ()
  "Check if in IPDB by comparing prompts."
  (save-excursion
    (let* ((case-fold-search nil)
           (last-ipdb-pos
            (progn
              (goto-char (point-max))
              (when (re-search-backward "^ipdb> " nil t)
                (point))))
           (last-ipy-pos
            (progn
              (goto-char (point-max))
              (when (re-search-backward "^In \\[[0-9]+\\]: " nil t)
                (point)))))
      (and last-ipdb-pos
           (or (not last-ipy-pos)
               (> last-ipdb-pos last-ipy-pos))))))

(defun epc--repl-check-if-in-embedded-ipython ()
  "Check if in embedded IPython instance."
  (save-excursion
    (let ((header-pos
           (progn
             (goto-char (point-max))
             (when
                 (re-search-backward "^In \\[[0-9]+\\]: embed()" nil t)
               (line-number-at-pos))))
          (footer-pos
           (progn
             (goto-char (point-max))
             (when (re-search-backward
                    "^Do you really want to exit \\(\\[y\\]\\/n\\)?"
                    nil t)
               (line-number-at-pos)))))
      (and header-pos
           (or (not footer-pos)
               (> header-pos footer-pos))))))

(defun epc-repl-switch-shell-to-ipython ()
  "Start IPython from normal terminal."
  (interactive)
  (when (epc-repl-check-if-in-shell)
    (term-send-raw-string "ipython\C-m")))

(defun epc-repl-switch-ipython-to-shell ()
  "Exit from IPython to normal terminal."
  (interactive)
  (when (epc-repl-check-if-in-ipython)
    (term-send-raw-string "exit\C-m")))

(defun epc-repl-switch-ipython-to-embed ()
  "Start embedded IPython from IPython."
  (interactive)
  (when (epc-repl-check-if-in-ipython)
    (term-send-raw-string "from IPython import embed\C-m")
    (term-send-raw-string "embed()\C-m")))

(defun epc-repl-switch-embed-to-ipython ()
  "Exit from embedded IPython to IPython."
  (interactive)
  (when (epc-repl-check-if-in-embedded-ipython)
    (term-send-raw-string "exit\C-m")))

(defun epc-repl-switch-ipython-to-ipdb ()
  "Start iPDB from IPython."
  (interactive)
  (when (epc-repl-check-if-in-ipython)
    (term-send-raw-string "import ipdb; ipdb.set_trace()\C-m")))

(defun epc-repl-switch-ipdb-to-ipython ()
  "Exit from iPDB to IPython."
  (interactive)
  (when (epc-repl-check-if-in-ipdb)
    (term-send-raw-string "exit\C-m")))


(provide 'epc-nav-repl)

(when
    (not load-file-name)
  (message "epc-nav-repl.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
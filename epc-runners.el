;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-01 19:07:14>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-python-config/epc-runners.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'epc-nav-repl)

(defun epc--runners-copy-region-or-buffer ()
  "Copy region or whole buffer to Python interpreter."
  (let ((content (if (region-active-p)
                     (buffer-substring-no-properties
                      (region-beginning) (region-end))
                   (buffer-substring-no-properties
                    (point-min) (point-max)))))
    (when (fboundp 'pim-fix-imports)
      (pim-fix-imports))
    (kill-new content)))

(defun epc--runners-paste-lines-to-vterm ()
  "Paste region (or buffer) to vterm, line by line."
  (interactive)
  (let* ((content (if (region-active-p)
                      (buffer-substring-no-properties
                       (region-beginning) (region-end))
                    (buffer-substring-no-properties (point-min)
                                                    (point-max))))
         (lines (split-string content "\n" t)))
    (dolist (line lines)
      (vterm-send-string line)
      (vterm-send-return))))

;; (defun epc-runners-paste-to-repl ()
;;   "Paste code into a Python REPL window without changing focus."
;;   (interactive)
;;   (let ((original-window (selected-window))
;;         target-window)
;;     (epc--runners-copy-region-or-buffer)
;;     (setq target-window (epc-repl-find-window original-window))
;;     (if target-window
;;         (with-selected-window target-window
;;           (cond
;;            ((epc-repl-check-if-in-shell)
;;             (epc-repl-switch-shell-to-ipython)
;;             (term-send-raw-string "paste\C-m"))
;;            ((epc-repl-check-if-in-ipython)
;;             (term-send-raw-string "paste\C-m"))
;;            ((epc-repl-check-if-in-embedded-ipython)
;;             (term-send-raw-string "paste\C-m"))
;;            ((epc-repl-check-if-in-ipdb)
;;             (term-send-raw-string "from IPython import embed\C-m")
;;             (term-send-raw-string "embed()\C-m")
;;             (term-send-raw-string "paste\C-m")
;;             (term-send-raw-string "exit\C-m"))))
;;       (message "No suitable Python REPL window found"))))

(defun epc-runners-paste-to-repl ()
  "Paste code into a Python REPL window without changing focus."
  (interactive)
  (let ((original-window (selected-window))
        (file-path (buffer-file-name))
        (target-window))
    (epc--runners-copy-region-or-buffer)
    (setq target-window (epc-repl-find-window original-window))

    (if target-window
        (with-selected-window target-window
          (cond
           ((epc-repl-check-if-in-shell)
            (epc-repl-switch-shell-to-ipython)
            (term-send-raw-string
             (format "__file__ = \"%s\"\C-m" file-path))
            (term-send-raw-string "paste\C-m"))

           ((epc-repl-check-if-in-ipython)
            (term-send-raw-string
             (format "__file__ = \"%s\"\C-m" file-path))
            (term-send-raw-string "paste\C-m"))

           ((epc-repl-check-if-in-embedded-ipython)
            (term-send-raw-string
             (format "__file__ = \"%s\"\C-m" file-path))
            (term-send-raw-string "paste\C-m"))

           ((epc-repl-check-if-in-ipdb)
            (term-send-raw-string "from IPython import embed\C-m")
            (term-send-raw-string "embed()\C-m")
            (term-send-raw-string
             (format "__file__ = \"%s\"\C-m" file-path))
            (term-send-raw-string "paste\C-m")
            (term-send-raw-string "exit\C-m"))))
      (message "No suitable Python REPL window found"))))

(defun epc-runners-paste-new-session ()
  "Paste code into new IPython session."
  (interactive)
  (epc-runners-switch-to-shell)
  (epc-runners-paste-to-repl))

(defun epc-runners-run-script ()
  "Run Python script in interactive terminal."
  (interactive)
  (let ((script-path (buffer-file-name)))
    (when script-path
      (other-window 1)
      (let ((repl-type (epc-repl-check-interactive-status)))
        (cond
         ((string= repl-type "shell")
          (term-send-raw-string (format "python %s\C-m" script-path)))
         ((string= repl-type "ipython")
          (term-send-raw-string (format "!python %s\C-m" script-path)))
         ((string= repl-type "embed")
          (epc-repl-switch-embed-to-ipython)
          (sleep-for 0.5)
          (term-send-raw-string (format "!python %s\C-m" script-path)))
         ((string= repl-type "ipdb")
          (epc-repl-switch-ipdb-to-ipython)
          (sleep-for 0.5)
          (term-send-raw-string (format "!python %s\C-m" script-path)))))
      (other-window -1))))

(defun epc-runners-run-script-with-ipdb ()
  "Run Python script in debug mode."
  (interactive)
  (let ((script-path (buffer-file-name))
        (run-cmd     (format "python -m ipdb %s\C-m" script-path)))
    (when script-path
      (other-window 1)
      (let ((repl-type (epc-repl-check-interactive-status)))
        (cond
         ((string= repl-type "shell")
          (term-send-raw-string run-cmd)
          (sleep-for 0.5)
          (term-send-raw-string "c\C-m"))
         ((string= repl-type "ipython")
          (epc-repl-switch-ipython-to-shell)
          (sleep-for 0.5)
          (term-send-raw-string run-cmd)
          (sleep-for 0.5)
          (term-send-raw-string "c\C-m"))
         ((string= repl-type "embed")
          (epc-repl-switch-embed-to-ipython)
          (epc-repl-switch-ipython-to-shell)
          (sleep-for 0.5)
          (term-send-raw-string run-cmd)
          (sleep-for 0.5)
          (term-send-raw-string "c\C-m"))
         ((string= repl-type "ipdb")
          (epc-repl-switch-ipdb-to-ipython)
          (epc-repl-switch-ipython-to-shell)
          (sleep-for 0.5)
          (term-send-raw-string run-cmd))))
      (other-window -1))))


(provide 'epc-runners)

(when
    (not load-file-name)
  (message "epc-runners.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-04-24 14:01:28>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-python-config/epc-imports.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(defun epc-imports-remove-unused-imports ()
  "Remove unused imports from the current buffer."
  (interactive)
  (save-excursion
    (pyimport-remove-unused)
    (goto-char (point-min))
    (while (re-search-forward
            "^from[ \t]+\\(?:\\w*\\s-*\\)import[ \t]+\\w+.*$" nil t)
      (replace-match ""))
    (while (re-search-forward
            "^from[ \t]+\\(?:\\w*\\s-*\\)cecreat import[ \t]+\\w+.*$"
            nil t)
      (replace-match ""))))

(provide 'epc-imports)

(when
    (not load-file-name)
  (message "epc-imports.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
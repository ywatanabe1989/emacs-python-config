;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-04-24 14:17:28>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-python-config/epc-navigation.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'cl-lib)

(defun epc-navigation-cycle (&optional step)
  "Cycle through buffers in python-mode."
  (interactive "p")
  (let* ((current-buffer (current-buffer))
         (python-buffers
          (seq-filter
           (lambda (buf)
             (with-current-buffer buf
               (eq major-mode 'python-mode)))
           (buffer-list)))
         (current-index
          (cl-position current-buffer python-buffers :test 'eq))
         (buffer-count (length python-buffers))
         (next-index (mod (+ current-index (or step 1)) buffer-count)))
    (when python-buffers
      (switch-to-buffer (nth next-index python-buffers)))))

(defun epc-navigation-toggle-source-test ()
  "Toggle between Python source and test files."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (is-test (string-match-p "/tests/" current-file))
         (target-path
          (if is-test
              (replace-regexp-in-string
               "/tests/" "/src/"
               (replace-regexp-in-string "test_" "" current-file))
            (replace-regexp-in-string
             "/src/" "/tests/"
             (concat (file-name-directory current-file)
                     "test_"
                     (file-name-nondirectory current-file))))))
    (if (file-exists-p target-path)
        (find-file target-path)
      (message "Target file does not exist: %s" target-path))))


(provide 'epc-navigation)

(when
    (not load-file-name)
  (message "epc-navigation.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
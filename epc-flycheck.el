;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-04-24 14:01:06>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-python-config/epc-flycheck.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'flycheck)

(defun epc-flycheck-in-region (region-start region-end)
  "Check for Flycheck errors in the specified region."
  (let* ((only-errors
          (seq-filter
           (lambda (err)
             (eq (flycheck-error-level err) 'error))
           flycheck-current-errors))
         (errors-in-region
          (seq-filter
           (lambda (err)
             (let ((err-start (flycheck-error-pos err)))
               (and (>= err-start region-start)
                    (<= err-start region-end))))
           only-errors)))
    errors-in-region))

(defun epc-flycheck-move-next-error-cyclic ()
  "Move to next Flycheck error cyclically in buffer."
  (interactive)
  (condition-case nil
      (flycheck-next-error)
    (error
     (goto-char (point-min))
     (flycheck-next-error))))

(defun epc-flycheck-setup ()
  "Configure flycheck for Python."
  (setq-default flycheck-disabled-checkers
                epc-disabled-checkers)
  (add-to-list 'flycheck-checkers 'python-flake8))

(provide 'epc-flycheck)

(when
    (not load-file-name)
  (message "epc-flycheck.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
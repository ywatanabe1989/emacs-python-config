;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-02 10:06:02>
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

(defcustom epc-flycheck-blacklist-filename-expressions
  '("__init__.py")
  "List of filename expressions to blacklist from flycheck setup.")

(defcustom epc-flycheck-whitelist-filename-expressions
  '()
  "List of filename expressions to always allow flycheck setup.")

(defun epc-flycheck--filename-match-p (expressions filename)
  "Return non-nil if FILENAME matches any entry in EXPRESSIONS, which may be strings or regexps."
  (seq-some
   (lambda (expr)
     (if (string-prefix-p "^" expr)
         (string-match-p expr filename)
       (string-match-p (regexp-quote expr) filename)))
   expressions))

(defun epc-flycheck-setup ()
  "Configure Flycheck for Python, honoring blacklist and whitelist."
  (interactive)
  (let ((filename (or (buffer-file-name) "")))
    (when (or (derived-mode-p 'python-mode)
              (and filename (string-match-p "\\.py\\'" filename)))
      (cond
       ;; Whitelist takes precedence over blacklist
       ((epc-flycheck--filename-match-p
         epc-flycheck-whitelist-filename-expressions filename)
        (flycheck-mode t)
        (message "File is explicitly whitelisted for flycheck."))
       ;; If blacklist matches, explicitly disable Flycheck
       ((epc-flycheck--filename-match-p
         epc-flycheck-blacklist-filename-expressions filename)
        (flycheck-mode -1)
        (message "File is blacklisted; disabling flycheck-mode."))
       ;; Normal setup
       (t
        (setq-local flycheck-disabled-checkers epc-disabled-checkers)
        (add-to-list 'flycheck-checkers 'python-flake8)
        (setq-local flycheck-executable-find
                    (lambda (cmd)
                      (or (executable-find cmd)
                          (let ((remote-venv-paths
                                 '("~/.env/bin/"
                                   "./venv/bin/"
                                   "./.env/bin/")))
                            (seq-find
                             (lambda (path)
                               (let
                                   ((full-path
                                     (expand-file-name
                                      (concat path cmd))))
                                 (and (file-exists-p full-path)
                                      full-path)))
                             remote-venv-paths)))))
        (flycheck-mode 1))
                                        ; Explicitly enable in this
       case
       (message "Flycheck configured for %s" filename)))))

;; (defun epc-flycheck-setup ()
;;   "Configure flycheck for Python."
;;   (setq-default flycheck-disabled-checkers
;;                 epc-disabled-checkers)
;;   (add-to-list 'flycheck-checkers 'python-flake8)

;;   ;; Set executable paths for remote sessions
;;   (setq-local flycheck-executable-find
;;               (lambda (cmd)
;;                 (or
;;                  ;; First try the default executable find
;;                  (executable-find cmd)
;;                  ;; Then try common virtualenv locations
;;                  (let ((remote-venv-paths
;;                         '("~/.env/bin/"
;;                           "./venv/bin/"
;;                           "./.env/bin/")))
;;                    (seq-find
;;                     (lambda (path)
;;                       (let
;;                           ((full-path
;;                             (expand-file-name (concat path cmd))))
;;                         (and (file-exists-p full-path)
;;                              full-path)))
;;                     remote-venv-paths))))))


(provide 'epc-flycheck)

(when
    (not load-file-name)
  (message "epc-flycheck.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
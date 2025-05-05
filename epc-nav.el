;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-01 19:07:31>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-python-config/epc-nav.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'cl-lib)

(defun epc-nav-cycle (&optional step)
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

(defun epc-nav-toggle-source-test ()
  "Toggle between Python source and test files or directories."
  (interactive)
  (cond
   ((eq major-mode 'dired-mode)
    (--epc-nav-toggle-source-test-directory))

   ((and (buffer-file-name)
         (or (eq major-mode 'python-mode)
             (string-match-p "\\.py$" (buffer-file-name))))
    (--epc-nav-toggle-source-test-file))

   (t
    (message "Not in a Python file or dired buffer"))))

(defun --epc-nav-toggle-source-test-file ()
  "Toggle between Python source and test files."
  (let* ((src-dir-pattern
          (car (car epc-nav-source-and-test-expressions-alist)))
         (test-dir-pattern
          (cdr (car epc-nav-source-and-test-expressions-alist)))
         (src-file-pattern
          (car (cadr epc-nav-source-and-test-expressions-alist)))
         (test-file-pattern
          (cdr (cadr epc-nav-source-and-test-expressions-alist)))
         (src-dir-regex (concat "/" src-dir-pattern "/"))
         (test-dir-regex (concat "/" test-dir-pattern "/"))
         (current-file (buffer-file-name))
         (is-test (string-match-p test-dir-regex current-file)))

    (if is-test
        ;; Test to Source - e.g., tests/test_file.py -> src/file.py
        (let* ((file-name (file-name-nondirectory current-file))
               (file-dir (file-name-directory current-file))
               (target-dir
                (replace-regexp-in-string test-dir-regex src-dir-regex
                                          file-dir))
               (target-file (replace-regexp-in-string
                             (concat "^" test-file-pattern)
                             ""
                             file-name))
               (target-path (concat target-dir target-file)))
          (if (file-exists-p target-path)
              (find-file target-path)
            (message "Target file does not exist: %s" target-path)))

      ;; Source to Test - e.g., src/file.py -> tests/test_file.py
      (let* ((file-name (file-name-nondirectory current-file))
             (file-dir (file-name-directory current-file))
             (target-dir
              (replace-regexp-in-string src-dir-regex test-dir-regex
                                        file-dir))
             (target-file (concat test-file-pattern file-name))
             (target-path (concat target-dir target-file)))
        (if (file-exists-p target-path)
            (find-file target-path)
          (message "Target file does not exist: %s" target-path))))))

;; (defun --epc-nav-toggle-source-test-file ()
;;   "Toggle between Python source and test files."
;;   (let* ((src-dir-pattern
;;           (car (car epc-nav-source-and-test-expressions-alist)))
;;          (test-dir-pattern
;;           (cdr (car epc-nav-source-and-test-expressions-alist)))
;;          (src-file-pattern
;;           (car (cadr epc-nav-source-and-test-expressions-alist)))
;;          (test-file-pattern
;;           (cdr (cadr epc-nav-source-and-test-expressions-alist)))
;;          (src-dir-regex (concat "/" src-dir-pattern "/"))
;;          (test-dir-regex (concat "/" test-dir-pattern "/"))
;;          (current-file (buffer-file-name))
;;          (is-test (string-match-p test-dir-regex current-file)))

;;     (if is-test
;;         ;; Test to Source
;;         (let ((target-path
;;                (replace-regexp-in-string
;;                 test-dir-regex src-dir-regex
;;                 (replace-regexp-in-string
;;                  (concat "^\\(.*\\)" test-file-pattern "\\(.*\\)$")
;;                  "\\1\\2"  ; Remove test_ prefix from filename
;;                  current-file nil nil 1))))
;;           (if (file-exists-p target-path)
;;               (find-file target-path)
;;             (message "Target file does not exist: %s" target-path)))

;;       ;; Source to Test
;;       (let* ((file-name (file-name-nondirectory current-file))
;;              (file-dir (file-name-directory current-file))
;;              (target-dir
;;               (replace-regexp-in-string src-dir-regex test-dir-regex
;;                                         file-dir))
;;              (target-file (concat test-file-pattern file-name))
;;              (target-path (concat target-dir target-file)))
;;         (if (file-exists-p target-path)
;;             (find-file target-path)
;;           (message "Target file does not exist: %s" target-path))))))

(defun --epc-nav-toggle-source-test-directory ()
  "Toggle between Python source and test directories in dired."
  (let*
      ((src-dir-pattern
        (car (car epc-nav-source-and-test-expressions-alist)))
       (test-dir-pattern
        (cdr (car epc-nav-source-and-test-expressions-alist)))
       (src-dir-regex (concat "/" src-dir-pattern "/"))
       (test-dir-regex (concat "/" test-dir-pattern "/"))
       (current-dir default-directory)
       (is-test (string-match-p test-dir-regex current-dir))
       (target-dir
        (if is-test
            (replace-regexp-in-string test-dir-regex src-dir-regex
                                      current-dir)
          (replace-regexp-in-string src-dir-regex test-dir-regex
                                    current-dir))))
    (if (file-directory-p target-dir)
        (dired target-dir)
      (message "Target directory does not exist: %s" target-dir))))


(provide 'epc-nav)

(when
    (not load-file-name)
  (message "epc-nav.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
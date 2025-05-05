;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-03 15:31:32>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-python-config/epc-templates.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(defvar epc-templates
  '(( "header"                  . epc-header-template)
    ( "import"                  . epc-import-template)
    ( "warnings"                . epc-warnings-template)
    ( "parameters"              . epc-parameters-template)
    ( "functions-and-classes"   . epc-functions-and-classes-template)
    ( "main-guard"              . epc-main-guard-template)
    ( "argparse"                . epc-argparse-template)
    ( "try-except"              . epc-try-except-template)
    ( "plot"                    . epc-plot-template)
    ( "ipdb"                    . epc-ipdb-template)
    ( "script"                  . epc-script-template)
    ( "init"                    . epc-init-template))
  "Template collection for Python code generation.")

(defvar epc-header-template
  "#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Timestamp: \"2024-11-03 10:33:13 (ywatanabe)\"
# File: placeholder.py

__FILE__ = \"placeholder.py\"

\"\"\"
Functionalities:
  - Does XYZ
  - Does XYZ
  - Does XYZ
  - Saves XYZ

Dependencies:
  - scripts:
    - /path/to/script1
    - /path/to/script2
  - packages:
    - package1
    - package2
IO:
  - input-files:
    - /path/to/input/file.xxx
    - /path/to/input/file.xxx

  - output-files:
    - /path/to/input/file.xxx
    - /path/to/input/file.xxx

(Remove me: Please fill docstrings above, while keeping the bulette point style, and remove this instruction line)
\"\"\"
"
  "Standard header template for Python files.
Includes shebang, encoding, timestamp, and structured docstring format.")

(defvar epc-import-template
  "\"\"\"Imports\"\"\"
import os
import sys
import argparse
"
  "Python template for mngs projects.")

(defvar epc-warnings-template
  "\"\"\"Warnings\"\"\"
# mngs.pd.ignore_SettingWithCopyWarning()
# warnings.simplefilter(\"ignore\", UserWarning)
# with warnings.catch_warnings():
#     warnings.simplefilter(\"ignore\", UserWarning)
"
  "Python template for mngs projects.")

(defvar epc-parameters-template
  "\"\"\"Parameters\"\"\"
# from mngs.io import load_configs
# CONFIG = load_configs()
"
  "Python template for mngs projects.")

(defvar epc-functions-and-classes-template
  "\"\"\"Functions & Classes\"\"\"
def main(args):
    return 0

import argparse
def parse_args() -> argparse.Namespace:
    \"\"\"Parse command line arguments.\"\"\"
    import mngs
    script_mode = mngs.gen.is_script()
    parser = argparse.ArgumentParser(description='')
    # parser.add_argument(
    #     \"--var\",
    #     \"-v\",
    #     type=int,
    #     choices=None,
    #     default=1,
    #     help=\"(default: %(default)s)\",
    # )
    # parser.add_argument(
    #     \"--flag\",
    #     \"-f\",
    #     action=\"store_true\",
    #     default=False,
    #     help=\"(default: %%(default)s)\",
    # )
    args = parser.parse_args()
    mngs.str.printc(args, c='yellow')
    return args

def run_main() -> None:
    \"\"\"Initialize mngs framework, run main function, and cleanup.\"\"\"
    global CONFIG, CC, sys, plt

    import sys
    import matplotlib.pyplot as plt
    import mngs

    args = parse_args()

    CONFIG, sys.stdout, sys.stderr, plt, CC = mngs.gen.start(
        sys,
        plt,
        args=args,
        file=__FILE__,
        sdir_suffix=None,
        verbose=False,
        agg=True,
    )

    exit_status = main(args)

    mngs.gen.close(
        CONFIG,
        verbose=False,
        notify=False,
        message=\"\",
        exit_status=exit_status,
    )
"
  "Python template for mngs projects.")

(defvar epc-main-guard-template
  "if __name__ == '__main__':
    run_main()

# EOF
"
  "Python template for mngs projects.")

(defvar epc-argparse-template
  "import argparse

def parse_args() -> argparse.Namespace:
    \"\"\"Parse command line arguments.\"\"\"
    import mngs
    script_mode = mngs.gen.is_script()
    parser = argparse.ArgumentParser(description='')
    # parser.add_argument(
    #     \"--var\",
    #     \"-v\",
    #     type=int,
    #     choices=None,
    #     default=1,
    #     help=\"(default: %(default)s)\",
    # )
    # parser.add_argument(
    #     \"--flag\",
    #     \"-f\",
    #     action=\"store_true\",
    #     default=False,
    #     help=\"(default: %%(default)s)\",
    # )
    args = parser.parse_args()
    mngs.str.printc(args, c='yellow')
    return args
"
  "Python template for mngs projects.")

(defvar epc-script-template
  (concat
   epc-header-template
   "\n"
   epc-import-template
   "\n"
   epc-warnings-template
   "\n"
   epc-parameters-template
   "\n"
   epc-functions-and-classes-template
   "\n"
   epc-main-guard-template
   )
  "Python template for mngs projects.")

(defvar epc-ipdb-template
  "__import__(\"ipdb\").set_trace()"
  "Python template for mngs projects.")

(defvar epc-try-except-template
  "try:
    \# YOUR CODE HERE
except Exception as e:
    print(e)
    __import__(\"ipdb\").set_trace()
"
  "Python template for mngs projects.")

(defvar epc-plot-template
  "fig, ax = mngs.plt.subplots()
fig, axes = mngs.plt.subplots()
axes_flat = axes.flatten()
"
  "Python template for mngs projects.")

(defvar epc-init-template
  "#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Timestamp: \"2024-10-22 19:51:47 (ywatanabe)\"
# File: %s

import os as __os
import importlib as __importlib
import inspect as __inspect

# Get the current directory
current_dir = __os.path.dirname(__FILE__)

# Iterate through all Python files in the current directory
for filename in __os.listdir(current_dir):
    if filename.endswith(\".py\") and not filename.startswith(\"__\"):
        module_name = filename[:-3]  # Remove .py extension
        module = __importlib.import_module(f\".{module_name}\", package=__name__)

        # Import only functions and classes from the module
        for name, obj in __inspect.getmembers(module):
            if __inspect.isfunction(obj) or __inspect.isclass(obj):
                if not name.startswith(\"_\"):
                    globals()[name] = obj

# Clean up temporary variables
del __os, __importlib, __inspect, current_dir, filename, module_name, module, name, obj

# EOF
"
  "Python template for __init__.py files in mngs projects.")

(defun epc-templates-insert-ipdb ()
  (interactive)
  (insert "__import__(\"ipdb\").set_trace()"))

(defun epc-templates-insert-template (template-name)
  "Insert predefined Python code template."
  (interactive
   (list (completing-read "Template: "
                          (mapcar #'car epc-templates))))
  (unless (eq major-mode 'python-mode)
    (error "Not in Python mode"))
  (let* ((template-pair (assoc template-name epc-templates))
         (template-symbol (cdr template-pair))
         (template (and template-symbol
                        (boundp template-symbol)
                        (symbol-value template-symbol)))
         (current-file (buffer-file-name)))
    (cond
     ((null template-pair)
      (error "Template %s not found" template-name))
     ((null template)
      (error "Template %s is not properly defined" template-name))
     ((string= template-name "init")
      (if current-file
          (insert (format template
                          (file-name-nondirectory current-file)))
        (error "Buffer not associated with file")))
     (t
      (insert template)))))

(defun epc-templates-insert-template-if-new-file ()
  "Auto-insert template for new Python files."
  (when (and (string= (file-name-extension
                       (or buffer-file-name ""))
                      "py")
             (= (point-min) (point-max)))
    (let* ((clean-path
            (if (string-match "/ssh:[^:]+:\\([^:]*\\)"
                              (buffer-file-name))
                (match-string 1 (buffer-file-name))
              (buffer-file-name))))
      (if (string= (file-name-nondirectory clean-path)
                   "__init__.py")
          (epc-templates-insert-template "init")
        (epc-templates-insert-template "script"))
      (goto-char (point-min)))))

(add-hook 'find-file-hook 'epc-templates-insert-template-if-new-file)


(provide 'epc-templates)

(when
    (not load-file-name)
  (message "epc-templates.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
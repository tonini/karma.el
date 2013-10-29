;;; karma.el --- karma Test Runner Emacs Integration
;;
;; Filename: karma.el
;; Description: karma Test Runner Emacs Integration
;; Author: Samuel Tonini
;; Maintainer: Samuel Tonini
;; Version: 0.1.0
;; URL: http://github.com/tonini/karma.el
;; Keywords: javascript, karma, testing

;; The MIT License (MIT)
;;
;; Copyright (c) Samuel Tonini
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;; this software and associated documentation files (the "Software"), to deal in
;; the Software without restriction, including without limitation the rights to
;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;; the Software, and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


;;; Commentary:
;;


;;; Code:

(require 'compile)

(defcustom karma-command "karma"
  "The shell command karma."
  :type 'string
  :group 'karma)

(defvar karma-start-buffer-name "*karma start*"
  "Name of the karma server output buffer.")

(defvar karma-run-buffer-name "*karma run*"
  "Name of the karma run output buffer.")

(defvar karma-buffer-name "*karma*"
  "Name of the karma server output buffer.")

(defvar karma--compilation-buffer-name nil
  "Used to store compilation name so recompilation works as expected.")
(make-variable-buffer-local 'karma--compilation-buffer-name)

(defvar karma--project-root-indicators
  '("package.json" "bower.json")
  "List of files which indicates a javascript project root.")

(defun karma-flatten (alist)
  "Flatten any lists within ALIST, so that there are no sublists."
  (cond ((null alist) nil)
        ((atom alist) (list alist))
        (t (append (karma-flatten (car alist))
                   (karma-flatten (cdr alist))))))

(defun karma-project-root ()
  (let ((file (file-name-as-directory (expand-file-name default-directory))))
    (karma--project-root-identifier file karma--project-root-indicators)))

(defun karma--project-root-identifier (file indicators)
  (let ((root-dir (if indicators (locate-dominating-file file (car indicators)) nil)))
    (cond (root-dir (directory-file-name (expand-file-name root-dir)))
          (indicators (karma--project-root-identifier file (cdr indicators)))
          (t nil))))

(defun karma-establish-root-directory ()
  "Set the default-directory to the karma used project root."
  (let ((project-root (karma-project-root)))
    (if (not project-root)
        (error "Couldn't find any project root")
      (setq default-directory project-root))))

(defun karma-build-compile-cmdlist (command)
  "Build the command list for `karma-compile' from COMMAND."
  (remove "" (karma-flatten
              (list karma-command (if (stringp command)
                                      (split-string command)
                                    command)))))

(define-compilation-mode karma-compilation-mode "Karma"
  "Karma compilation mode.")

(defun karma-compile (name cmdlist)
  "In a buffer identified by NAME, run CMDLIST in `karma-compilation-mode'."
  (save-some-buffers (not compilation-ask-about-save)
                     (when (boundp 'compilation-save-buffers-predicate)
                       compilation-save-buffers-predicate))
  (let* ((karma--compilation-buffer-name name))
    (with-current-buffer
        (compilation-start (format "%s" cmdlist) 'karma-compilation-mode
                           (lambda (b) karma--compilation-buffer-name)))))

(defun karma-config-path-to-file ()
  (let ((file (format "%s/%s" (karma-project-root) ".karma")))
    (if (not (file-exists-p file))
        (error "Couldn't find any karma config file."))
    (with-temp-buffer
      (insert-file-contents file)
      (expand-file-name (buffer-string)))))

(defun karma-start-with-prompt (args)
  (interactive "Mkarma start: ")
  (karma-execute (list "start" args) karma-start-buffer-name))

(defun karma-start-single-run ()
  (interactive)
  (karma-execute (list "start" (karma-config-path-to-file) "--single-run")
                 karma-start-buffer-name))

(defun karma-start ()
  (interactive)
  (karma-execute (list "start" (karma-config-path-to-file))
                 karma-start-buffer-name))

(defun karma-run-with-prompt (args)
  (interactive "Mkarma run: ")
  (karma-execute (list "run" args) karma-run-buffer-name))

(defun karma-run ()
  (interactive)
  (karma-execute (list "run") karma-run-buffer-name))

(defun karma-execute (cmdlist &optional buffer-name)
  "Run a karma command with CMDLIST as arguments."
  (interactive "Mkarma: ")
  (let ((name (if buffer-name buffer-name
                karma-buffer-name)))
    (karma-establish-root-directory)
    (karma-compile name (karma-build-compile-cmdlist cmdlist))))

(provide 'karma)

;;; karma.el ends here

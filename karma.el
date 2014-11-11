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

;;; Code:

(require 'compile)
(require 'ansi-color)
(require 'json)

(defcustom karma-config-file ".karma"
  ""
  :type 'string
  :group 'karma)

(defun karma-command ()
  "Return the shell command for karma from the .karma config file"
  (expand-file-name (gethash "karma-command" (karma-project-config)) (karma-project-root)))

(defun karma-config-file-path ()
  "Return the path to the karma config file"
  (expand-file-name (gethash "config-file" (karma-project-config)) (karma-project-root)))

(defun karma-project-config ()
  (let* ((json-object-type 'hash-table)
         (karma-config (json-read-from-string
                        (with-temp-buffer
                          (insert-file-contents (format "%s/%s" (karma-project-root) karma-config-file))
                          (buffer-string)))))
    karma-config))

(defvar karma-start-buffer-name "*karma start*"
  "Name of the karma server output buffer.")

(defvar karma-run-buffer-name "*karma run*"
  "Name of the karma run output buffer.")

(defvar karma-server-buffer-name "*karma server*"
  "Name of the karma server output buffer.")

(defun karma--flatten (alist)
  (cond ((null alist) nil)
        ((atom alist) (list alist))
        (t (append (karma--flatten (car alist))
                   (karma--flatten (cdr alist))))))

(defun karma--build-runner-cmdlist (command)
  "Build the commands list for the runner."
  (remove "" (karma--flatten
              (list (if (stringp command)
                        (split-string command)
                      command)))))

(defvar karma--project-root-indicators
  '("package.json" "bower.json")
  "list of file-/directory-names which indicate a root of a elixir project")

(defun karma-project-root ()
  (let ((file (file-name-as-directory (expand-file-name default-directory))))
    (karma--project-root-identifier file karma--project-root-indicators)))

(defun karma--project-root-identifier (file indicators)
  (let ((root-dir (if indicators (locate-dominating-file file (car indicators)) nil)))
    (cond (root-dir (directory-file-name (expand-file-name root-dir)))
          (indicators (karma--project-root-identifier file (cdr indicators)))
          (t nil))))

(defun karma--establish-root-directory ()
  "Set the default-directory to the karma used project root."
  (let ((project-root (karma-project-root)))
    (if (not project-root)
        (error "Couldn't find any project root")
      (setq default-directory project-root))))

(defvar karma-buffer--buffer-name nil
  "Used to store compilation name so recompilation works as expected.")
(make-variable-buffer-local 'karma-buffer--buffer-name)

(defvar karma-buffer--error-link-options
  '(karma "\\([-A-Za-z0-9./_]+\\):\\([0-9]+\\)\\(: warning\\)?" 1 2 nil (3) 1)
  "File link matcher for `compilation-error-regexp-alist-alist' (matches path/to/file:line).")

(defun karma-buffer--kill-any-orphan-proc ()
  "Ensure any dangling buffer process is killed."
  (let ((orphan-proc (get-buffer-process (buffer-name))))
    (when orphan-proc
      (kill-process orphan-proc))))

(define-compilation-mode karma-buffer-mode "Karma"
  "Karma compilation mode."
  (progn
    (font-lock-add-keywords nil
                            '(("^Finished in .*$" . font-lock-string-face)
                              ("^Karma.*$" . font-lock-string-face)))
    ;; Set any bound buffer name buffer-locally
    (setq karma-buffer--buffer-name karma-buffer--buffer-name)
    (set (make-local-variable 'kill-buffer-hook)
         'karma-buffer--kill-any-orphan-proc)))

(defvar karma-buffer--save-buffers-predicate
  (lambda ()
    (not (string= (substring (buffer-name) 0 1) "*"))))

(defun karma-buffer--handle-compilation-once ()
  (remove-hook 'compilation-filter-hook 'karma-buffer--handle-compilation-once t)
  (delete-matching-lines "\\(-*- mode:\\|^$\\|karma run\\|Loading config\\|--no-single-run\\|Karma finished\\|Karma started\\|karma-compilation;\\)"
                         (point-min) (point)))

(defun karma-buffer--handle-compilation ()
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun karma-compilation-run (cmdlist buffer-name)
  "Run CMDLIST in `buffer-name'.
Returns the compilation buffer.
Argument BUFFER-NAME for the compilation."
  (save-some-buffers (not compilation-ask-about-save) karma-buffer--save-buffers-predicate)
  (let* ((karma-buffer--buffer-name buffer-name)
         (compilation-filter-start (point-min)))
    (with-current-buffer
        (compilation-start (mapconcat 'shell-quote-argument cmdlist " ")
                           'karma-buffer-mode
                           (lambda (b) karma-buffer--buffer-name))
      (setq-local compilation-error-regexp-alist-alist
                  (cons karma-buffer--error-link-options compilation-error-regexp-alist-alist))
      (setq-local compilation-error-regexp-alist (cons 'karma compilation-error-regexp-alist))
      (add-hook 'compilation-filter-hook 'karma-buffer--handle-compilation nil t)
      (add-hook 'compilation-filter-hook 'karma-buffer--handle-compilation-once nil t))))

(defun karma-start ()
  (interactive)
  (karma-execute (list "start" (karma-config-file-path))
                 karma-start-buffer-name))

(defun karma-start-single-run ()
  (interactive)
  (karma-execute (list "start" (karma-config-file-path) "--single-run")
                 karma-start-buffer-name))

(defun karma-start-no-single-run (&optional args)
  (interactive "P")
  (karma-execute (list "start" (karma-config-file-path) "--no-single-run")
                 karma-start-buffer-name))

(defun karma-run ()
  (interactive)
  (karma-execute (list "run" (karma-config-file-path))
                 karma-run-buffer-name))

(defun karma-execute (cmdlist buffer-name)
  "Run a karma command."
  (let ((old-directory default-directory))
    (karma--establish-root-directory)
    (message default-directory)
    (karma-compilation-run (karma--build-runner-cmdlist (list (karma-command) cmdlist))
                           buffer-name)
    (cd old-directory)))

;;;###autoload
(define-minor-mode karma-mode
  "Toggle karma mode.

Key bindings:
\\{karma-mode-map}"
  nil
  ;; The indicator for the mode line.
  " karma"
  :group 'karma
  :global nil)

(provide 'karma)

;;; karma.el ends here

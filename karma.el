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

(defvar karma--javascript-project-root-indicator
  "package.json"
  "List of files which indicates a javascript project root.")

(provide 'karma)

;;; karma.el ends here

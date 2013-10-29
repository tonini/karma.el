;;; karma-tests.el --- karma.el ert unit tests
;;
;; Author: Samuel Tonini
;; Maintainer: Samuel Tonini
;; Description: karma Test Runner Emacs Integration
;; Maintainer: Samuel Tonini
;; URL: https://github.com/tonini/karma.el

;; This file is NOT part of GNU Emacs.

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

(require 'ert)
(require 'test-helper)

(ert-deftest test-karma-project-root/npm-file-exists ()
  (within-sandbox "lib/npm"
   (f-touch "../../package.json")
   (should (equal (karma-project-root) karma-sandbox-path))))

(ert-deftest test-karma-project-root/bower-file-exists ()
  (within-sandbox "lib/bower"
   (f-touch "../../bower.json")
   (should (equal (karma-project-root) karma-sandbox-path))))

(ert-deftest test-karma-project-root/npm-file-dont-exists ()
  (within-sandbox
   (should (equal (karma-project-root) nil))))

(provide 'karma-tests)

;;; karma-tests.el ends here

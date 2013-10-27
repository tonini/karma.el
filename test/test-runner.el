;; Usage:
;;
;;   emacs -Q -l test/test-runner.el           # interactive mode
;;   emacs -batch -Q -l test/test-runner.el    # batch mode

(let ((current-directory (file-name-directory load-file-name)))
  (setq karma-test-path (expand-file-name "." current-directory))
  (setq karma-root-path (expand-file-name ".." current-directory)))

(add-to-list 'load-path karma-root-path)
(add-to-list 'load-path karma-test-path)

(require 'karma)

(dolist (test-file (or argv (directory-files karma-test-path t "-tests.el$")))
  (load test-file nil t))

(ert-run-tests-batch-and-exit t)

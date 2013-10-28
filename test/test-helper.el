(require 'f)

(defvar karma-test-path
  (f-dirname (f-this-file)))

(defvar karma-sandbox-path
  (f-expand "sandbox" karma-test-path))

(defmacro with-sandbox (&rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(let ((default-directory karma-sandbox-path))
     (f-mkdir karma-sandbox-path)
     ,@body
     (f-delete karma-sandbox-path :force)))

(provide 'test-helper)

(require 'f)

(defvar karma-test-path
  (f-dirname (f-this-file)))

(defvar karma-sandbox-path
  (f-expand "sandbox" karma-test-path))

(defmacro within-sandbox (&optional current &rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(let ((default-directory
           (f-join karma-sandbox-path (format "%s" ,current))))
     (f-mkdir karma-sandbox-path)
     ,@body
     (f-delete karma-sandbox-path :force)))

(provide 'test-helper)

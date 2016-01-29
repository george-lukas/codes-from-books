;;;; cap19.lisp - Beyond Exception Handling: Conditions and Restarts
;;;; George Lukas < last update: Jan/01/2016 - 02:05PM >

(define-condition malformed-log-entry-error (error)
  ((text :initarg :text :reader text)))

(defun parse-log-file (file)
  (with-open-file (in file :direction :input)
    (loop :for text := (read-line in nil nil) :while text
       :for entry := (restart-case (parse-log-entry text)
		       (skip-log-entry () nil))
       :when entry :collect it)))

(defun log-analyzer ()
  (handler-bind ((malformed-log-entry-error
		  #'(lambda (c)
		      (use-value
		       (make-instance 'malformed-log-entry :text (text c))))))
    (dolist (log (find-all-logs))
      (analyze-log log))))

(defun skip-log-entry (c)
  (let ((restart (find-restart 'skip-log-entry)))
    (when restart (invoke-restart restart))))


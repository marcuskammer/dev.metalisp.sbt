(defmacro main (&body body)
  `(spinneret:with-html
     (:main ,@body)))

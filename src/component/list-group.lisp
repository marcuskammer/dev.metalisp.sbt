;; List groups
;; Create lists of content in a card with a flush list group.

(defmacro list-group-item (&body body)
  `(spinneret:with-html
     (:li :class "list-group-item" ,@body)))

(defmacro list-group (&rest rest)
  `(spinneret:with-html
     (:ul :class "list-group list-group-flush"
          ,@(loop for item in rest
	          collect (destructuring-bind (&key content) item
	        	    `(list-group-item  ,content))))))

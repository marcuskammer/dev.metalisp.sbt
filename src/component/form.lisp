(defpackage cl-sbt/form
  (:use
   :cl)
  (:export
   :form-ctrl))

(in-package :cl-sbt/form)

(defmacro form-ctrl (&rest rest)
  `(spinneret:with-html
     ,@(loop for item in rest
             collect (destructuring-bind (&key id label type placeholder) item
                       `(progn
                          (:label :for ,id :class "form-label" ,label)
                          (:input :type ,type :class "form-control" :id ,id :placeholder ,placeholder))))))

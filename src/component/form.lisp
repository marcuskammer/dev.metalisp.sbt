(defpackage cl-sbt/form
  (:use
   :cl)
  (:export
   :ctrl
   :ctrl-col
   :select))

(in-package :cl-sbt/form)

(defmacro ctrl (&rest rest)
  `(spinneret:with-html
     ,@(loop for item in rest
             collect (destructuring-bind (&key id label type placeholder text describeby) item
                       `(:div :class "mb-3"
                              (:label :for ,id
                                      :class "form-label"
                                      ,label)
                              ,(if describeby
                                   `(:input :type ,type
                                            :class "form-control"
                                            :id ,id
                                            :placeholder ,placeholder
                                            :aria-describeby ,describeby)
                                   `(:input :type ,type
                                            :class "form-control"
                                            :id ,id
                                            :placeholder ,placeholder))
                              ,(if text
                                   `(:div :id ,describeby
                                          :class "form-text"
                                          ,text)
                                   nil))))))

(defmacro ctrl-col (&rest rest)
  `(spinneret:with-html
     ,@(loop for item in rest
             collect (destructuring-bind (&key id label type placeholder) item
                       `(:div :class "row g-3 align-items-center"
                              (:div :class "col-auto"
                                    (:label :for ,id
                                            :class "col-form-label"
                                            ,label))
                              (:div :class "col-auto"
                                    (:label :type ,type
                                            :id ,id
                                            :class "form-control"))
                              (:div :class "col-auto"
                                    (:span :class "form-text"
                                           ,text)))))))

(defmacro select ((&key size) &rest rest)
  `(spinneret:with-html
     (:select :class ,(concatenate 'string
                                   "form-select"
                                   (if size (format nil " form-select-~a" size)))
       :aria-label "Default select example"
       (:option :selected "Open this selected menu")
       ,@(loop for item in rest
               collect (destructuring-bind (&key content value) item
                         `(:option :value ,value ,content))))))

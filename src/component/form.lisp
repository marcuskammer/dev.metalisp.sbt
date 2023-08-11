(defpackage cl-sbt/form
  (:use
   :cl)
  (:export
   :ctrl
   :ctrl-col))

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

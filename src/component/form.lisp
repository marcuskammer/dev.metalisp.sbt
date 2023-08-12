(defpackage cl-sbt/form
  (:use
   :cl)
  (:export
   :ctrl
   :ctrl-col
   :select
   :select-option))

(in-package :cl-sbt/form)

(defmacro ctrl (&rest rest)
  `(spinneret:with-html
     ,@(loop for item in rest
             collect (destructuring-bind (&key id label type placeholder text describeby) item
                       `(:div :class "mb-3"
                              (:label :for ,id
                                      :class "form-label"
                                      ,label)
                              (:input :type ,type
                                      :class "form-control"
                                      :id ,id
                                      :placeholder ,placeholder
                                      ,@(when (stringp describeby) (list :aria-describeby describeby)))
                              ,(if (stringp text)
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

(defmacro select-option (&rest rest)
  `(spinneret:with-html
     ,@(loop for item in rest
             collect (destructuring-bind (&key content value) item
                       `(:option :value ,value ,content)))))

(defmacro select ((&key size) &body body)
  (let ((class-attr (cond
                      ((null size) "form-select")
                      ((numberp size) "form-select")
                      ((and (stringp size)
                            (string= size "multiple")) "form-select")
                      ((stringp size)
                       (format nil "form-select form-select-~a" size))
                      (t (error "Invalid size specification: ~a" size)))))
    `(spinneret:with-html
       (:select :class ,class-attr
         ,@(when (numberp size) `(:size ,size))
         ,@(when (and (stringp size) (string= size "multiple")) (list :multiple t))
         :aria-label "Default select example"
         (:option :selected t "Open this selected menu")
         ,@body))))

(in-package :cl-sbt-album)

(defmacro main (&body body)
  `(spinneret:with-html
     (:main ,@body)))

(defmacro hero ((&key (headline nil) (cta-1 nil) (cta-2 nil)) &body body)
  `(spinneret:with-html
     (:section :class "py-5 text-center container"
               (:div :class "row py-lg-5"
                     (:div :class "col-lg-6 col-md-8 mx-auto"
                           (:h1 :class "fw-light"
                                ,headline)
                           (:p :class "lead text-body-secondary"
                               ,@body)
                           (:p (:a :class "btn btn-primary my-2"
                                   ,cta-1)
                               (:a :class "btn btn-secondary my-2"
                                   ,cta-2)))))))

(defmacro card (&body body)
  `(spinneret:with-html
     (:div :class "col"
           (:div :class "card shadow-sm"
                 (:div :class "card-body"
                       (:p :class "card-text"
                           ,@body))))))

(defmacro album (&body body)
  `(spinneret:with-html
     (:div :class "album py-5 bg-body-tertiary"
           (:div :class "container"
                 (:div :class "row row-cols-1 row-cols-sm-2 row-cols-md-3 g-3"
                       ,@body)))))

(defun show-album ()
  (album (dolist (card-text '("foo" "bar" "hello" "world"))
           (card card-text))))

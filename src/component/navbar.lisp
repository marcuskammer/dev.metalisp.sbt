(defmacro navbar-brand ((&key (logo-src nil)) &body body)
  `(spinneret:with-html
       (:div :class "container-fluid"
             (:a :class "navbar-brand"
                 :href "#"
                 ,(if (null logo-src)
                      nil
                      (:img :src logo-src
                            :alt "Logo"
                            :width 30
                            :height 24
                            :class "d-inline-block align-text-top"))
                 ,@body))))

(defmacro navbar-nav ())

(defmacro navbar-toggler ())

(defmacro navbar-text ())

(defmacro navbar (&body body)
  `(spinneret:with-html
     (:nav :class "navbar"
           ,@body)))

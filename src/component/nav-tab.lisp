;; https://getbootstrap.com/docs/5.3/components/navs-tabs/

;; Navigation available in Bootstrap share general markup and styles, from the
;; base .nav class to the active and disabled states. Swap modifier classes to
;; switch between each style.

;; The base .nav component is built with flexbox and provide a strong foundation
;; for building all types of navigation components. It includes some style
;; overrides (for working with lists), some link padding for larger hit areas, and
;; basic disabled styling.

(in-package :cl-sbt)

(defmacro nav-item (name active url)
  `(spinneret:with-html
     (:li :class "nav-item"
	  (:a :class ,(if (null active) "nav-link" "nav-link active")
              :href (format nil "#~a" ,url)
              ,name))))

(defmacro nav ((&key (style nil)) &rest rest)
  `(spinneret:with-html
     (:ul :class ,(if (null style) "nav" (concatenate 'string "nav " style))
	  ,@(loop for tab in rest
	          collect (destructuring-bind (&key name active url) tab
	        	    `(nav-item ,name ,active ,url))))))

;; ;;
;; ;; For testing purposes
;; ;;
;; (defmacro show-nav-fn (name style)
;;   "Generate a function of NAME and STYLE to test the different bootstrap
;; navigation styles"
;;   `(defun ,name ()
;;      (nav (:style ,style)
;;           (:title "Active" :active t :url "#")
;;           (:title "Link" :url "#")
;;           (:title "Link" :url "#"))))

;; (show-nav-fn show-nav-ha "justify-content-center")
;; (show-nav-fn show-nav-ra "justify-content-end")
;; (show-nav-fn show-nav-va "flex-column")
;; (show-nav-fn show-nav-tabs "nav-tabs")
;; (show-nav-fn show-nav-pills "nav-pills")
;; (show-nav-fn show-nav-underline "nav-underline")

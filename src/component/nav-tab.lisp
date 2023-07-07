(in-package :cl-sbt)

(defun tabpanel-class (active fade)
  (concatenate 'string "tab-pane "  (if fade " fade " nil) (if active " in active " nil)))

(defmacro nav-item (title active url)
  `(spinneret:with-html
     (:li :role "presentation"
          :class ,(if active "active" "")
	  (:a :href (format nil "#~a" ,url)
              :data-toggle "tab" ,title))))

(defmacro nav ((&key (tabs nil)) &rest rest)
  `(spinneret:with-html
     (:ul :class "nav"
          :role "tablist"
	  ,@(loop for tab in rest
		  collect (destructuring-bind (&key title active id content) tab
			    `(tab-nav  ,title  ,active  ,id))))))

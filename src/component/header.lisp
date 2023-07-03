;; Collection of page headers

(in-package :cl-sbt)

;; <header data-bs-theme="dark">
;;   <div class="text-bg-dark collapse" id="navbarHeader" style="">
;;     <div class="container">
;;       <div class="row">
;;         <div class="col-sm-8 col-md-7 py-4">
;;           <h4>About</h4>
;;           <p class="text-body-secondary">Add some information about the album below, the author, or any other background context. Make it a few sentences long so folks can pick up some informative tidbits. Then, link them off to some social networking sites or contact information.</p>
;;         </div>
;;         <div class="col-sm-4 offset-md-1 py-4">
;;           <h4>Contact</h4>
;;           <ul class="list-unstyled">
;;             <li><a href="#" class="text-white">Follow on Twitter</a></li>
;;             <li><a href="#" class="text-white">Like on Facebook</a></li>
;;             <li><a href="#" class="text-white">Email me</a></li>
;;           </ul>
;;         </div>
;;       </div>
;;     </div>
;;   </div>
;;   <div class="navbar navbar-dark bg-dark shadow-sm">
;;     <div class="container">
;;       <a href="#" class="navbar-brand d-flex align-items-center">
;;         <svg xmlns="http://www.w3.org/2000/svg" width="20" height="20" fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2" aria-hidden="true" class="me-2" viewBox="0 0 24 24"><path d="M23 19a2 2 0 0 1-2 2H3a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h4l2-3h6l2 3h4a2 2 0 0 1 2 2z"></path><circle cx="12" cy="13" r="4"></circle></svg>
;;         <strong>Album</strong>
;;       </a>
;;       <button class="navbar-toggler collapsed" type="button" data-bs-toggle="collapse" data-bs-target="#navbarHeader" aria-controls="navbarHeader" aria-expanded="false" aria-label="Toggle navigation">
;;         <span class="navbar-toggler-icon"></span>
;;       </button>
;;     </div>
;;   </div>
;; </header>

(defvar header-contact-examples
  '(("Follow on Twitter" . "foo")
    ("Like on Facebook" . "foo")
    ("Email me" . "foo")))

(defvar header-about-example
  "Add some information about the album below, the author, or any other background context. Make it a few sentences long so folks can pick up some informative tidbits. Then, link them off to some social networking sites or contact information.")

(defmacro header-navheader-about (&body body)
  `(spinneret:with-html
     (:div :class "col-sm-8 col-md-7 py-4"
           (:h4 "About")
           (:p :class "text-body-secondary"
               ,@body))))

(defmacro header-navheader-contact (&body body)
  `(spinneret:with-html
     (:div :class "col-sm-4 offset-md-1 py-4"
           (:h4 "Contact")
           (:ul :class "list-unstyled"
                ,@body))))

(defmacro header-navheader (&body body)
  `(spinneret:with-html
     (:div :class "text-bg-dark collapse"
           :id "navbarHeader"
           (:div :class "container"
                 (:div :class "row"
                       ,@body)))))

;; (defun show-header-about (about)
;;   (header-about about))

(defun show-header-navheader-contact (contacts)
  (header-navheader-contact (dolist (contact contacts)
                              (:li (:a :href (rest contact)
                                       :class "text-white"
                                       (first contact))))))

(defun show-header-navheader (about contacts)
  (header-navheader (header-navheader-about about)
    (show-header-navheader-contact contacts)))

(defmacro header (&body body)
  `(spinneret:with-html
     (:header ,@body)))

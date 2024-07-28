;;;; -*- mode: lisp; coding: utf-8; fill-column: 84; indent-tabs-mode: nil; -*-
;;;; main.lisp
;;;; Provide general functions.

(defpackage ml-sbt
  (:use :cl)
  (:import-from #:spinneret #:*html-lang*)
  (:export
   :*l10n*
   :find-l10n
   :*use-cdn*
   :*cdn-url-css*
   :*cdn-url-js*
   :*local-css-url*
   :*local-url-js*
   :*bs-version*
   :*color-theme*
   :bs-url-css
   :bs-url-js
   :download-bs-css
   :download-bs-js
   :write-html-to-file
   :with-page
   :body-header
   :body-main
   :remove-special-chars
   :clean-form-str
   :build-str-name
   :build-str-value
   :build-str-value-prop
   :build-str-class
   :build-str-id))

(in-package :ml-sbt)

(defparameter *document-root-dir*
  "public/"
  "Defines directory name for use in document-root to serve static files.")

(defparameter spinneret:*fill-column* 120)

(defparameter *bs-version* "5.3.2")

(defparameter *use-cdn* t)

(defparameter *cdn-url-css*
  (concatenate 'string
               "https://cdn.jsdelivr.net/npm/bootstrap@"
               *bs-version*
               "/dist/css/bootstrap.min.css"))

(defparameter *cdn-url-js*
  (concatenate 'string
               "https://cdn.jsdelivr.net/npm/bootstrap@"
               *bs-version*
               "/dist/js/bootstrap.bundle.min.js"))

(defparameter *bs-path*
  (concatenate 'string
               "/"
               *bs-version*
               "/"))

(defparameter *local-url-css*
  (concatenate 'string
               *bs-path*
               "bootstrap.min.css")
  "Constructs path for HTML to load local bootstrap css from disk.")

(defparameter *local-url-js*
  (concatenate 'string
               *bs-path*
               "bootstrap.bundle.min.js")
  "Constructs path for HTML to load local bootstrap js from disk.")

(defparameter *color-theme* "light")

(defparameter *print-css-url* "/print.css")

(defun bs-url-css ()
  (if *use-cdn*
      *cdn-url-css*
      *local-url-css*))

(defun bs-url-js ()
  (if *use-cdn*
      *cdn-url-js*
      *local-url-js*))

(defun bs-download-full-path ()
  (truename (concatenate 'string *document-root-dir* *bs-version*)))

(defun download-file (url directory)
  "Downloads a file from a given URL and saves it to the specified directory."
  (let* ((filename (car (last (uiop:split-string url :separator "/"))))
         (filepath (merge-pathnames filename directory)))
    (ensure-directories-exist directory)
    (let ((content (dex:get url)))
      (with-open-file (stream filepath
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (write-string content stream)))
    filepath))

(defmacro define-download-function (name url directory)
  `(defun ,name (&optional (directory ,directory))
     (download-file ,url directory)))

(define-download-function download-bs-css *cdn-url-css* (bs-download-full-path))
(define-download-function download-bs-js *cdn-url-js* (bs-download-full-path))

(defun write-html-str-to-file (filename string
                               &key (lang "en") (style :tree) (fc 120))
  (let ((spinneret:*html-lang* lang)
        (spinneret:*html-style* style)
        (spinneret:*fill-column* fc))
    (with-open-file (stream filename :direction :output :if-exists :supersede)
      (write-string string stream))))

(defmacro with-page ((&key
                        meta
                        (title "Web page")
                        main-con
                        add-css-urls
                        add-js-urls)
                     &body body)
  "This macro simplifies the process of creating an HTML web page.

META: The meta-information for the web page.

TITLE: Specifies the title of the web page. Defaults to 'Web page'.

MAIN-CON: If t add css class `container` to <main>.

ADD-CSS-URLS: An optional parameter for additional CSS file URLs.

ADD-JS-URLS: An optional parameter for additional JavaScript file URLs.

BODY: Denotes the markup for the body of the web page.

Example usage:
   (with-page (:meta (:author \"John Doe\") :title \"My Page\" :main-con t) \"foo\")"
  `(spinneret:with-html-string
     (:doctype)
     (:html :data-bs-theme ,*color-theme*
            (:head (:meta :charset "utf-8")
                   (:meta :name "viewport"
                          :content "width=device-width, initial-scale=1")
                   ,@(loop for (key value) on meta by #'cddr
                           collect `(:meta :name
                                           ,(string-downcase (symbol-name key))
                                           :content ,(getf meta key)))

                   (:title ,title)

                   (:link :type "text/css" :rel "stylesheet" :href ,*print-css-url*)
                   (:link :type "text/css" :rel "stylesheet" :href ,(bs-url-css))
                   ,@(loop for url in add-css-urls
                           collect `(:link :type "text/css"
                                      :rel "stylesheet" :href ,url))

                   (:script (:raw "(()=>{'use strict';const getStoredTheme=()=>localStorage.getItem('theme');const getPreferredTheme=()=>{const storedTheme=getStoredTheme();if(storedTheme){return storedTheme}return window.matchMedia('(prefers-color-scheme: dark)').matches?'dark':'light'};const setTheme=theme=>{if(theme==='auto'){document.documentElement.setAttribute('data-bs-theme',(window.matchMedia('(prefers-color-scheme: dark)').matches?'dark':'light'))}else{document.documentElement.setAttribute('data-bs-theme',theme)}};setTheme(getPreferredTheme())})();")))

            (:body :class "py-3" ,@body)

            (:script :src ,(bs-url-js))
            ,@(loop for url in add-js-urls
                    collect `(:script :src ,url)))))

(defmacro body-header (main-heading &body body)
  `(spinneret:with-html
     (:header
       (:div :class "container text-center py-3"
             (:a :href "#main-content"
                 :class "skip-link"
                 (find-l10n "skip-link" *html-lang* *l10n*)))
       ,@body
       (:div :class "container"
             (:h1 ,main-heading)))))

(defmacro body-main (&optional main-con &body body)
  `(spinneret:with-html
       (:main :id "main-content"
              ,@(when main-con (list :class "container"))
              ,@body)))


(defun remove-special-chars (str)
  "Removes all special characters from the string STR except numbers and alphabets.

STR: The input string from which special characters need to be removed.

Example:
  (remove-special-chars \"a1b!@#$%^&*()c2\") will return \"a1bc2\"

Returns:
  A new string with special characters removed."
  (remove-if-not (lambda (char)
                   (or (alpha-char-p char) (digit-char-p char)))
                 str))

(defun clean-form-str (str)
  "Cleans a form string for use as a name or identifier.

STR: The string to clean. Removes leading and trailing spaces, replaces spaces
with dashes, and converts to lowercase.

Returns:
  A new string which can be used as HTML class."
  (string-downcase (substitute #\- #\Space (string-trim '(#\Space) str))))

(defun build-str-name (name)
  "Builds a standardized string by adding a 'group-' prefix and applying cleaning
functions.

NAME: The initial name string.

Returns:
  A new standardized string."
  (concatenate 'string "group-" (clean-form-str name)))

(defun build-str-value (value)
  "Trims leading and trailing spaces from the given value string.

VALUE: The string to be cleaned.

Returns:
  A new string without leading and trailing spaces."
  (string-trim '(#\Space) value))

(defun build-str-value-prop (value)
  "Builds a value property string by applying various cleaning functions.

VALUE: The initial value string.

Returns:
  A new value property string."
  (clean-form-str (build-str-value value)))

(defun build-str-class (class name)
  "Builds a class string by concatenating 'form-check-label' and a standardized
name string.

CLASS: Corresponding class property.

NAME: The initial name string.

Returns:
  A new class string."
  (concatenate 'string class " " (build-str-name name)))

(defun build-str-id (name value)
  "Builds an ID string by concatenating a standardized name string and a sanitized
value property string.

NAME: The initial name string.

VALUE: The initial value string.

Returns:
  A new ID string."
  (concatenate 'string
               (build-str-name name)
               "-"
               (remove-special-chars (build-str-value-prop value))))

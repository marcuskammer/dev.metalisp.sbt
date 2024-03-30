;;;; -*- mode: lisp; coding: utf-8; fill-column: 84; indent-tabs-mode: nil; -*-
;;;; main.lisp
;;;; Provide general functions.

(defpackage dev.metalisp.sbt
  (:use :cl)
  (:export
   :*l10n*
   :find-l10n
   :*use-cdn*
   :*cdn-css-url*
   :*cdn-js-url*
   :*local-css-url*
   :*local-js-url*
   :*bs-version*
   :*color-theme*
   :download-bs-css
   :download-bs-js
   :write-html-to-file
   :with-page
   :remove-special-chars
   :clean-form-str
   :build-name-str
   :build-value-str
   :build-value-prop-str
   :build-class-str
   :build-id-str))

(in-package :dev.metalisp.sbt)

(defparameter spinneret:*fill-column* 120)

(defparameter *bs-version* "5.3.2")

(defparameter *use-cdn* t)

(defparameter *cdn-css-url*
  (concatenate 'string
               "https://cdn.jsdelivr.net/npm/bootstrap@"
               *bs-version*
               "/dist/css/bootstrap.min.css"))

(defparameter *cdn-js-url*
  (concatenate 'string
               "https://cdn.jsdelivr.net/npm/bootstrap@"
               *bs-version*
               "/dist/js/bootstrap.bundle.min.js"))

(defparameter *bs-path*
  (concatenate 'string
               "public/"
               *bs-version*
               "/"))

(defparameter *local-css-url*
  (concatenate 'string
               *bs-path*
               "bootstrap.min.css"))

(defparameter *local-js-url*
  (concatenate 'string
               *bs-path*
               "bootstrap.bundle.min.js"))

(defparameter *color-theme* "dark")

(defun bs-css-url ()
  (if *use-cdn*
      *cdn-css-url*
      *local-css-url*))

(defun bs-js-url ()
  (if *use-cdn*
      *cdn-js-url*
      *local-js-url*))

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

(define-download-function download-bs-css *cdn-css-url* *bs-path*)
(define-download-function download-bs-js *cdn-js-url* *bs-path*)

(defun write-html-str-to-file (filename string
                               &key (lang "en") (style :tree) (fc 120))
  (let ((spinneret:*html-lang* lang)
        (spinneret:*html-style* style)
        (spinneret:*fill-column* fc))
    (with-open-file (stream filename :direction :output :if-exists :supersede)
      (write-string string stream))))

(defmacro with-page ((&key meta (title "Web page") main-con add-css-urls
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
  `(spinneret:with-html
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

                   (:link :type "text/css" :rel "stylesheet" :href ,(bs-css-url))
                   ,@(loop for url in add-css-urls
                           collect `(:link :type "text/css"
                                      :rel "stylesheet" :href ,url)))

            (:body (:h1 :class "visually-hidden" ,title)
              (:main ,@(when main-con (list :class "container")) ,@body)

              (:script :src ,(bs-js-url))
              ,@(loop for url in add-js-urls
                      collect `(:script :src ,url))))))

(defun remove-special-chars (str)
  "Removes all special characters from the string STR except numbers and alphabets.

STR: The input string from which special characters need to be removed.

Example:
  (remove-special-chars \"a1b!@#$%^&*()c2\") will return \"a1bc2\"

Returns:
  A new string with special characters removed."
  (remove-if-not #'(lambda (char)
                     (or (alpha-char-p char) (digit-char-p char)))
                 str))

(defun clean-form-str (str)
  "Cleans a form string for use as a name or identifier.

STR: The string to clean. Removes leading and trailing spaces, replaces spaces
with dashes, and converts to lowercase.

Returns:
  A new string which can be used as HTML class."
  (string-downcase (substitute #\- #\Space (string-trim '(#\Space) str))))

(defun build-name-str (name)
  "Builds a standardized string by adding a 'group-' prefix and applying cleaning
functions.

NAME: The initial name string.

Returns:
  A new standardized string."
  (concatenate 'string "group-" (clean-form-str name)))

(defun build-value-str (value)
  "Trims leading and trailing spaces from the given value string.

VALUE: The string to be cleaned.

Returns:
  A new string without leading and trailing spaces."
  (string-trim '(#\Space) value))

(defun build-value-prop-str (value)
  "Builds a value property string by applying various cleaning functions.

VALUE: The initial value string.

Returns:
  A new value property string."
  (clean-form-str (build-value-str value)))

(defun build-class-str (class name)
  "Builds a class string by concatenating 'form-check-label' and a standardized
name string.

CLASS: Corresponding class property.

NAME: The initial name string.

Returns:
  A new class string."
  (concatenate 'string class " " (build-name-str name)))

(defun build-id-str (name value)
  "Builds an ID string by concatenating a standardized name string and a sanitized
value property string.

NAME: The initial name string.

VALUE: The initial value string.

Returns:
  A new ID string."
  (concatenate 'string
               (build-name-str name)
               "-"
               (remove-special-chars (build-value-prop-str value))))

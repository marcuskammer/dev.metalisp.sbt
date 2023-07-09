;; https://getbootstrap.com/docs/5.3/components/card/

;; A card is a flexible and extensible content container. It includes options for
;; headers and footers, a wide variety of content, contextual background colors,
;; and powerful display options. If you’re familiar with Bootstrap 3, cards
;; replace our old panels, wells, and thumbnails. Similar functionality to those
;; components is available as modifier classes for cards.

(in-package :cl-sbt)

(defmacro card-title (&body body)
  "Card titles are used by adding .card-title to a <h*> tag."
  `(spinneret:with-html
     (:h5 :class "card-title" ,@body)))

(defmacro card-subtitle (&body body)
  `(spinneret:with-html
     (:h6 :class "card-subtitle mb-2 text-body-secondary" ,@body)))

(defmacro card-text (&body body)
  `(spinneret:with-html
     (:p :class "card-text" ,@body)))

(defmacro card-link (&body body)
  `(spinneret:with-html
     (:a :href "#" :class "card-link" ,@body)))

(defmacro card-header (&body body)
  `(spinneret:with-html
     (:div :class "header" ,@body)))

(defmacro card-img ((&key (src nil) (alt nil)))
  ".card-img-top places an image to the top of the card."
  `(spinneret:with-html
     (:img :src ,src
           :alt ,alt
           :class "card-img-top")))

(defmacro card-body (&body body)
  `(spinneret:with-html
     (:div :class "card-body" ,@body)))

(defmacro card ((&key (img-src nil)) &body body)
  `(spinneret:with-html
     (:div :class "card"
           ,(if (null img-src) nil `(:img :class "card-img-top" :src ,img-src))
           (:div :class "card-body" ,@body))))

(defmacro card-* (&body body)
  `(spinneret:with-html
     (:div :class "card" ,@body)))

;; Kitchen sink
;; Mix and match multiple content types to create the card you need, or throw
;; everything in there. Shown below are image styles, blocks, text styles, and a
;; list group—all wrapped in a fixed-width card.

(defun card-kitchen-sink ()
  (card-* (card-img (:src "..."))
    (card-body (card-title "Card title")
               (card-text "Some quick example text to build on the card title and make up the bulk of the card's content."))
    (list-group (:content "An item")
                (:content "A second item")
                (:content "A third item"))
    (card-body (card-link "Card Link")
               (card-link "Another Link"))))

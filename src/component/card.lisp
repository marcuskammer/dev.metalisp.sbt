;; https://getbootstrap.com/docs/5.3/components/card/

;; A card is a flexible and extensible content container. It includes options for
;; headers and footers, a wide variety of content, contextual background colors,
;; and powerful display options. If you’re familiar with Bootstrap 3, cards
;; replace our old panels, wells, and thumbnails. Similar functionality to those
;; components is available as modifier classes for cards.

(in-package :cl-sbt)

(defmacro card ((&key (img-src nil) (btn-txt nil)) &body body)
  `(spinneret:with-html
     (:div :class "card"
           ,(when img-src `(:img :class "card-img-top" :src ,img-src))
           (:div :class "card-body"
                 (:p :class "card-text"
                     ,@body)
                 ,(when btn-txt `(btn-primary ,btn-txt))))))

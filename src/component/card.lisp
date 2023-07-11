;; https://getbootstrap.com/docs/5.3/components/card/

;; A Bootstrap Card is a flexible and extensible content container that includes
;; multiple options for headers and footers, a variety of content, contextual
;; background colors, and powerful display options. Introduced in Bootstrap 4,
;; cards are designed to replace old components such as panels, wells, and
;; thumbnails.

;; Cards are made up of multiple parts and can include:

;; Card Title: Serves as the header for the card and is typically used to include
;; the name or the main purpose of the card.

;; Card Subtitle: Usually follows the title and provides additional context or
;; information.

;; Card Text: This is the main content area for the card. It can include text,
;; links, buttons, and more.

;; Card Image: An optional image can be included at the top of a card.

;; Card Link: This is typically a call to action or a way to direct the user to
;; more information.

;; Card Header and Card Footer: These optional sections can be added to the top or
;; bottom of a card.

;; In terms of design, cards provide built-in Bootstrap styles, such as borders
;; and flexbox, along with optional headers and footers. They offer a lot of
;; flexibility and can be used in a wide range of designs and layouts.

(defpackage cl-sbt-card
  (:use :cl)
  (:export
   :title
   :subtitle
   :text
   :link
   :header
   :img
   :body
   :card-with-img
   :card))

(in-package :cl-sbt-card)

(defmacro title (&body body)
"This macro generates a Bootstrap card title.

BODY: The contents of the title."

  `(spinneret:with-html
       (:h5 :class "card-title" ,@body)))

(defmacro subtitle (&body body)
"This macro generates a Bootstrap card subtitle.

BODY: The contents of the subtitle."

  `(spinneret:with-html
       (:h6 :class "card-subtitle mb-2 text-body-secondary" ,@body)))

(defmacro text (&body body)
"This macro generates a Bootstrap card text.

BODY: The contents of the text."

  `(spinneret:with-html
       (:p :class "card-text" ,@body)))

(defmacro link ((&key (href "#")) &body body)
"This macro generates a Bootstrap card link.

HREF: The URL that the link will point to.

BODY: The contents of the link."

  `(spinneret:with-html
       (:a :href ,href :class "card-link" ,@body)))

(defmacro header (&body body)
"This macro generates a Bootstrap card header.

BODY: The contents of the header."

  `(spinneret:with-html
       (:div :class "header" ,@body)))

(defmacro img ((&key (src "#") (alt "Card Image")))
"This macro generates a Bootstrap card image.

SRC: The URL of the image.

ALT: The alt text for the image."

  `(spinneret:with-html
       (:img :src ,src
             :alt ,alt
             :class "card-img-top")))

(defmacro body (&body body)
"This macro generates a Bootstrap card body.

BODY: The contents of the body."

  `(spinneret:with-html
       (:div :class "card-body" ,@body)))

(defmacro card-with-img ((&key (img-src nil)) &body body)
"This macro generates a Bootstrap card with an image.

IMG-SRC: The URL of the image.

BODY: The contents of the body."

  `(spinneret:with-html
       (:div :class "card"
             ,(if (null img-src) nil `(img (:src ,img-src)))
             (:div :class "card-body" ,@body))))

(defmacro card (&body body)
"This macro generates a Bootstrap card.

BODY: The contents of the card."

  `(spinneret:with-html
       (:div :class "card" ,@body)))

;; Kitchen sink
;; Mix and match multiple content types to create the card you need, or throw
;; everything in there. Shown below are image styles, blocks, text styles, and a
;; list group—all wrapped in a fixed-width card.

(defun card-kitchen-sink ()
  (card (img (:src "..."))
        (body (card-title "Card title")
              (card-text "Some quick example text to build on the card title and make up the bulk of the card's content."))
        (cl-sbt-list-group (:content "An item")
                           (:content "A second item")
                           (:content "A third item"))
        (body (card-link "Card Link")
              (card-link "Another Link"))))

;; https://getbootstrap.com/docs/5.3/components/navs-tabs/

;; The Bootstrap Navs-Tabs component is a versatile navigation feature that
;; provides different styles of navigation elements. It's used to create sets
;; of nav links, which are presented in a series of tabs or pill-like buttons,
;; often used for switching between pages or sections in the same page without
;; a page refresh.

;; Bootstrap's Nav-Tabs component includes two key types:

;; Tabs: They are horizontal navigation elements that are styled to look like
;; physical tabs. They are commonly used for in-page navigation where the
;; selection of one tab changes the content of the page without requiring a
;; full page refresh.

;; Pills: These are similar to tabs, but they have a different visual style.
;; They appear as rounded rectangles, hence the name "pills". They serve the
;; same purpose as tabs, providing in-page navigation, but their unique styling
;; can help them to stand out more on the page or fit a specific design
;; aesthetic.

;; Nav-Tabs component also supports dropdowns and justified tabs or pills.

;; Dropdowns: These are special type of navigation elements that reveal
;; additional navigation options when clicked or hovered over. They are useful
;; for providing access to multiple links from a single nav item.

;; Justified tabs/pills: When you want your nav links to occupy the same
;; horizontal space, you can justify the links to distribute evenly across
;; their parent container.

;; Bootstrap's Nav-Tabs component is mobile-friendly and responsive, meaning it
;; adjusts its layout based on the size of the user's screen. This makes it a
;; flexible choice for a variety of web layouts.

(defpackage cl-sbt/nav
  (:use :cl)
  (:export
   :item
   :nav))

(in-package :cl-sbt/nav)

(defmacro item (name active url)
  "This macro generates a navigation item for a Bootstrap navigation list.

NAME: Specifies the name of the navigation item.
ACTIVE: Specifies whether the navigation item should be marked as active.
URL: Specifies the URL that the navigation item will link to.

Example:
 (nav-item \"Home\" t \"home\")"
  `(spinneret:with-html
     (:li :class "nav-item"
	  (:a :class ,(if (null active) "nav-link" "nav-link active")
              :href (format nil "#~a" ,url)
              ,name))))

(defmacro nav ((&key (style nil)) &rest rest)
  "This macro generates a Bootstrap navigation list.

STYLE: Specifies the style of the navigation list. If not provided, the default Bootstrap style is used.
REST: Specifies a list of navigation items. Each item is a plist with the following keys:
- :name: Specifies the name of the navigation item.
- :active: Specifies whether the navigation item should be marked as active.
- :url: Specifies the URL that the navigation item will link to.

Example:
 (nav (:style \"nav-pills\")
      (:name \"Home\" :active t :url \"home\")
      (:name \"About\" :url \"about\")
      (:name \"Contact\" :url \"contact\"))"
  `(spinneret:with-html
     (:ul :class ,(concatenate 'string "nav " style)
	  ,@(loop for tab in rest
	          collect (destructuring-bind (&key name active url) tab
	        	    `(nav-item ,name ,active ,url))))))

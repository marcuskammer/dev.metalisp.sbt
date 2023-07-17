;; Bootstrap's grid system allows you to create flexible and responsive layouts
;; through a series of containers, rows, and columns. It uses a 12-column
;; layout as a base, but allows for combining columns to create wider sections.

;; Here are the main aspects of Bootstrap's grid system:

;; Containers: These are the most basic layout element in Bootstrap and are
;; required to wrap site contents. There are two types of containers available:
;; container (which sets a max-width at each responsive breakpoint) and
;; container-fluid (which is always full width).

;; Rows: Rows are horizontal groups of columns that ensure your columns are
;; lined up properly due to the flexbox layout underneath. They must be placed
;; within a container for proper alignment and padding.

;; Columns: Columns are the immediate child of rows. Content should be placed
;; within columns, and only columns may be immediate children of rows. They can
;; be customized based on different screen sizes (referred to as breakpoints in
;; Bootstrap) using the appropriate classes (like .col-md-6 for medium
;; screens). Columns create gutters (gaps between column content) via padding.

;; Breakpoints: Bootstrap has five responsive breakpoints; these are xs (extra
;; small, less than 576px), sm (small, greater than 576px), md (medium, greater
;; than 768px), lg (large, greater than 992px), and xl (extra large, greater
;; than 1200px). Each of these represent a media query, ensuring your layout
;; adapts to the viewing environment.

;; Offsetting and Ordering: Bootstrap also provides classes to offset and
;; reorder your columns, allowing more granular control over your layout.

;; Nesting: Columns can be nested, meaning you can have a column be a grid
;; container for additional columns.

;; The grid system is based on flexbox, ensuring flexibility and feature
;; richness. From mobile to desktop, the grid system will scale up
;; appropriately to ensure your designs are fully responsive.

(defpackage cl-sbt/grid
  (:use :cl)
  (:export
   :container
   :row
   :col))

(in-package :cl-sbt/grid)

(defun make-container-class (name value default-class)
  "Generates a Bootstrap container class string for a particular breakpoint.

NAME is the name of the breakpoint (e.g., 'xs', 'sm', 'md', etc.).
VALUE is non-nil when the container's width is set for the breakpoint.
DEFAULT-CLASS is a string that represents the default container class.

The function generates a ' container-NAME' class string if VALUE is non-nil.

Example:
  (make-container-class \"md\" t \"container\") ; => \" container-md\""
  (if value (format nil " ~a-~a" default-class name)
      ""))

(defun make-row-class (name value)
  "Generates a Bootstrap row class string for a particular breakpoint or a general column setting.

NAME is the name of the breakpoint (e.g., 'xs', 'sm', 'md', etc.), or 'cols' for a general setting.
VALUE is an integer that specifies the number of equal-width columns at the given breakpoint or in general.

The function generates a 'row-cols-NAME-VALUE' class string. If NAME is 'cols',
it omits the NAME part.

Examples:
  (make-row-class \"md\" 3) ; => \" row-cols-md-3\"
  (make-row-class \"cols\" 2) ; => \" row-cols-2\""
  (if value
      (if (string-equal "cols" name)
          (format nil " row-cols-~d" value)
          (format nil " row-cols-~a-~d" name value))
      ""))

(defun make-col-class (name size-offset-pair)
  "Generates a Bootstrap column class string for a particular breakpoint.

NAME is the name of the breakpoint (e.g., 'xs', 'sm', 'md', etc.).
SIZE-OFFSET-PAIR is a list that contains the size and optional offset for the column at the given breakpoint.

The function generates a ' col-NAME-SIZE offset-NAME-OFFSET' class string. If
the size or offset is nil, it omits the corresponding part.

Examples:
  (make-col-class \"md\" '(3 1)) ; => \" col-md-3 offset-md-1\"
  (make-col-class \"lg\" '(4 nil)) ; => \" col-lg-4\""
  (if size-offset-pair
      (let ((size (first size-offset-pair))
            (offset (second size-offset-pair)))
        (concatenate 'string
                     (if size (format nil " col-~a-~d" name size) "")
                     (if offset (format nil " offset-~a-~d" name offset) "")))
      ""))

(defun breakpoint-class (&key (kind "container") (xs nil) (sm nil) (md nil) (lg nil) (xl nil) (xxl nil))
  (let ((xs-str (cond
                  ((eq kind :container) (make-container-class "xs" xs "container"))
                  ((eq kind :row) (make-row-class "xs" xs))
                  ((eq kind :col) (make-col-class "xs" xs))))
        (sm-str (cond
                  ((eq kind :container) (make-container-class "sm" sm "container"))
                  ((eq kind :row) (make-row-class "sm" sm))
                  ((eq kind :col) (make-col-class "sm" sm))))
        (md-str (cond
                  ((eq kind :container) (make-container-class "md" md "container"))
                  ((eq kind :row) (make-row-class "md" md))
                  ((eq kind :col) (make-col-class "md" md))))
        (lg-str (cond
                  ((eq kind :container) (make-container-class "lg" lg "container"))
                  ((eq kind :row) (make-row-class "lg" lg))
                  ((eq kind :col) (make-col-class "lg" lg))))
        (xl-str (cond
                  ((eq kind :container) (make-container-class "xl" xl "container"))
                  ((eq kind :row) (make-row-class "xl" xl))
                  ((eq kind :col) (make-col-class "xl" xl))))
        (xxl-str (cond
                   ((eq kind :container) (make-container-class "xxl" xxl "container"))
                   ((eq kind :row) (make-row-class "xxl" xxl))
                   ((eq kind :col) (make-col-class "xxl" xxl)))))
    (concatenate 'string xs-str sm-str md-str lg-str xl-str xxl-str)))

(defmacro container ((&key (fluid nil) (breakpoint nil) (text nil)) &body body)
  "Generates a Bootstrap container.

FLUID: When non-nil, the container becomes fluid (full width).
XS, SM, MD, LG, XL, XXL: Specify the size of the container at various breakpoints.

Example:
  (container (:fluid t :sm t)
    (col (:md (6 nil)) \"Hello, world!\"))

This will generate a fluid container with a medium-sized column inside it,
containing the text 'Hello, world!'."
  `(spinneret:with-html
     (:div :class
           ,(concatenate 'string
                         (if (null fluid) "container" "container-fluid")
                         (if (null breakpoint) ""
                             (apply #'breakpoint-class breakpoint))
                         (if (null text) ""
                             (apply #'cl-sbt/utility:text text)))
           ,@body)))

(defmacro row ((&key (breakpoint nil) (cols nil) (align-items nil) (justify-content nil)) &body body)
  "Generates a Bootstrap row.

XS, SM, MD, LG, XL, XXL: Specify the number of equal-width columns for extra small, small, medium, large, extra large, and extra extra large devices respectively.
COLS: Specifies the number of columns irrespective of the viewport or breakpoint size.

Examples:
  (row (:xs 2) \"Hello, world!\")
  ; Creates a row with two equal-width columns for extra small devices,
  ; containing the text 'Hello, world!'

  (row (:sm 4 :md 3 :lg 2) \"Hello, world!\")
  ; Creates a row with four equal-width columns for small devices, three for
  ; medium devices, and two for large devices, containing the text 'Hello, world!'

  (row (:cols 3) \"Hello, world!\")
  ; Creates a row with three equal-width columns irrespective of the viewport
  ; or breakpoint size, containing the text 'Hello, world!'

This will generate a row element with Bootstrap classes based on the given
arguments, containing the specified body content."
  `(spinneret:with-html
     (:div :class
           ,(string-downcase
             (concatenate 'string
                          "row"
                          (if (null breakpoint) ""
                              (apply #'breakpoint-class breakpoint))
                          (make-row-class "cols" cols)
                          (if (null align-items) "" (format nil " align-items-~a" align-items))
                          (if (null justify-content) "" (format nil " justify-content-~a" justify-content))))
           ,@body)))

(defmacro col ((&key (breakpoint nil) (col nil) (align-self nil) (spacing nil)) &body body)
  "Generates a Bootstrap column.

COL: Specifies the number of columns the element spans.

XS, SM, MD, LG, XL, XXL: List that specify the number of columns the element
spans and optional offset at various breakpoints.

ALIGN-SELF: Specifies the alignment of the column. Possible values are 'start',
'center', 'end'.

SPACING: A list specifying the Bootstrap spacing class. The list should contain
keyword arguments that can be passed to the cl-sbt-spacing:spacing function.

Example:
  (col (:col 6 :md (8 2) :align-self :center) \"Hello, world!\")
  ; This will generate a column that spans 6 columns by default, 8 medium-sized
  ; columns with an offset of 2 medium-sized columns, and aligns its content in the
  ; center. The column contains the text 'Hello, world!'."
  `(spinneret:with-html
     (:div :class
           ,(concatenate 'string
                         (if (null col) "col" (format nil "col-~d" col))
                         (if (null breakpoint) ""
                             (apply #'breakpoint-class breakpoint))
                         (if (null align-self) "" (string-downcase (format nil " align-self-~a" align-self)))
                         (if (null spacing) ""
                             (apply #'cl-sbt/utility:spacing spacing)))
           ,@body)))

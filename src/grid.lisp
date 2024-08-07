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

(defpackage ml-sbt/grid
  (:use :cl)
  (:import-from
   :ml-sbt/utility
   :*breakpoints*)
  (:export
   :con
   :row
   :col))

(in-package :ml-sbt/grid)

(defun string-clean (str)
  (string-trim " " (string-downcase str)))

(defun make-con-class (name value)
  "Generates a Bootstrap container class string for a particular breakpoint.

NAME is the name of the breakpoint (e.g., 'xs', 'sm', 'md', etc.).

The function generates a ' container-NAME' class string if VALUE is non-nil.

Example:
  (make-container-class \"md\" t) ; => \" container-md\""
  (if value
      (format nil "container-~a " name)
      ""))

(defun make-row-class (name value)
  "Generates a Bootstrap row class string for a particular breakpoint or a general column setting.

NAME is the name of the breakpoint (e.g., 'xs', 'sm', 'md', etc.), or 'cols'
for a general setting.

VALUE is an integer that specifies the number of equal-width columns at the
given breakpoint or in general.

The function generates a 'row-cols-NAME-VALUE' class string. If NAME is 'cols',
it omits the NAME part.

Examples:
  (make-row-class \"md\" 3) ; => \" row-cols-md-3\"
  (make-row-class \"cols\" 2) ; => \" row-cols-2\""
  (if value
      (if (string-equal "cols" name)
          (format nil "row-cols-~d " value)
          (format nil "row-cols-~a-~d " name value))
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
  (let* ((size (first size-offset-pair))
         (offset (second size-offset-pair))
         (size-string (if size
                          (if (string= name "")
                              (format nil "col-~d " size)
                              (format nil "col-~a-~d " name size))
                          ""))
         (offset-string (if offset
                            (if (string= name "")
                                (format nil "offset-~d " offset)
                                (format nil "offset-~a-~d " name offset))
                            "")))
    (concatenate 'string size-string offset-string)))

(defun breakpoint-class (&key (kind "con") (xs nil) (sm nil) (md nil) (lg nil) (xl nil) (xxl nil))
  "Generates Bootstrap class string for different kinds of elements across breakpoints.

The KIND argument specifies the type of element for which the classes are generated.
It can be :con for container, :row for row, or :col for column.

XS, SM, MD, LG, XL, XXL arguments represent the Bootstrap breakpoints.
They can be used to specify specific classes for each breakpoint.
The value of these arguments depends on the KIND of element:
  - For :con, they should be non-nil if the container's width is set for the corresponding
    breakpoint.
  - For :row, they should be an integer representing the number of equal-width columns at the
    corresponding breakpoint.
  - For :col, they should be a list that contains the size and optional offset for the column
    at the corresponding breakpoint.

The function generates the corresponding Bootstrap classes for the given breakpoints and KIND.

Examples:
  (breakpoint-class :kind \"con\" :md t)
  ; will generate a string for a medium container.

  (breakpoint-class :kind \"row\" :sm 2 :md 3)
  ; will generate a string for a row with 2 columns on small devices and 3 columns on medium devices.

  (breakpoint-class :kind \"col\" :lg '(4 1))
  ; will generate a string for a column that spans 4 out of 12 grid units and is offset by 1 on large devices.

  (breakpoint-class :kind \"con\" :xs t :sm t :md t :lg t :xl t :xxl t)
  ; will generate a string for a fluid container that spans all breakpoints."
  (assert (and kind
               (or xs sm md lg xl xxl)))
  (let ((breakpoint-values (list xs sm md lg xl xxl)))
    (string-clean
     (apply #'concatenate 'string
            (loop for i from 0 below (length *breakpoints*)
                  for breakpoint-name = (nth i *breakpoints*)
                  for breakpoint-value = (nth i breakpoint-values)
                  collect
                  (cond
                    ((equal kind "con") (make-con-class breakpoint-name breakpoint-value))
                    ((equal kind "row") (make-row-class breakpoint-name breakpoint-value))
                    ((equal kind "col") (make-col-class breakpoint-name breakpoint-value))))))))

(defmacro con ((&key (fluid nil) (breakpoint nil) (text nil) (spacing nil)) &body body)
  "Generates a Bootstrap container.

FLUID: When non-nil, the container becomes fluid (full width).

BREAKPOINT: Specifies the size of the container at various breakpoints, should
be 'sm', 'md', 'lg', 'xl', or 'xxl'.

TEXT: A keyword list that applies text utilities. -> ml-sbt/utility:text

Example 1:
  (con (:fluid t) \"Hello, world!\")
  ; This will generate a full width (fluid) container with the text 'Hello, world!'.

Example 2:
  (con (:breakpoint (:kind \"con\" :md t)) \"Welcome to the site!\")
  ; This will generate a container that is medium-sized at the defined
  ; breakpoint, containing the text 'Welcome to the site!'.

Example 3:
  (con (:text (:alignment \"center\")) \"Center-aligned text!\")
  ; This will generate a container with center-aligned text.

Example 4:
  (con (:fluid t :breakpoint (:kind \"con\" :sm t) :text (:weight \"bold\")) \"Bold text in a small fluid container!\")
  ; This will generate a full width (fluid) container that is small at the
  ; defined breakpoint, containing bold text."
  (let* ((fluid-str (if (null fluid) "container " "container-fluid "))
         (breakpoint-str (if (null breakpoint) "" (format nil "~a " (apply #'breakpoint-class breakpoint))))
         (text-str (if (null text) "" (format nil "~a " (apply #'ml-sbt/utility:text text))))
         (spacing-str (if (null spacing) "" (apply #'ml-sbt/utility:spacing spacing)))
         (class-str (string-clean (concatenate 'string fluid-str breakpoint-str text-str spacing-str))))
    `(spinneret:with-html
       (:comment "START CONTAINER")
       (:div :class ,class-str
             ,@body)
       (:comment "END CONTAINER"))))

(defmacro row ((&key (cols nil) (breakpoint nil) (alignitems nil) (justifycontent nil) (spacing nil)) &body body)
  "Generates a Bootstrap row.

COLS: Specifies the number of columns irrespective of the viewport or
breakpoint size.

BREAKPOINT: Specifies the number of equal-width columns at various
breakpoints. It should be 'xs', 'sm', 'md', 'lg', 'xl', or 'xxl'.

ALIGNITEMS: Specifies the vertical alignment of columns. It can be :start,
'center', 'end', 'stretch', or 'baseline'.

JUSTIFYCONTENT: Specifies the horizontal alignment of columns. It can be
'start', 'center', 'end', 'around', or 'between'.

Example 1:
  (row (:breakpoint (:kind \"row\" :xs 2)) \"Hello, world!\")
  ; Creates a row with two equal-width columns for extra small devices,
  ; containing the text 'Hello, world!'

Example 2:
  (row (:breakpoint (:kind \"row\" :sm 4 :md 3 :lg 2)) \"Hello, world!\")
  ; Creates a row with four equal-width columns for small devices, three for
  ; medium devices, and two for large devices, containing the text 'Hello, world!'

Example 3:
  (row (:cols 3) \"Hello, world!\")
  ; Creates a row with three equal-width columns irrespective of the viewport
  ; or breakpoint size, containing the text 'Hello, world!'

Example 4:
  (row (:alignitems \"center\" :justifycontent \"between\") \"Hello, world!\")
  ; Creates a row with centered items that are evenly distributed in the
  ; horizontal direction, containing the text 'Hello, world!'"
  (let ((class-str (string-clean
                    (concatenate 'string
                                 "row "
                                 (if (null cols) "" (make-row-class "cols" cols))
                                 (if (null breakpoint) ""
                                     (apply #'breakpoint-class breakpoint))
                                 (if (null alignitems) "" (format nil "align-items-~a " alignitems))
                                 (if (null justifycontent) "" (format nil "justify-content-~a " justifycontent))
                                 (if (null spacing) ""
                                     (apply #'ml-sbt/utility:spacing spacing))))))
    `(spinneret:with-html
       (:div :class ,class-str
             ,@body))))

(defmacro col ((&key (cols nil) (breakpoint nil) (alignself nil) (spacing nil)) &body body)
  "Generates a Bootstrap column.

COLS: Specifies the number of columns the element spans.

BREAKPOINT: List that specifies the number of columns the element spans and
optional offset at various breakpoints. It should be 'xs', 'sm', 'md', 'lg', 'xl',
or 'xxl'.

ALIGN-SELF: Specifies the alignment of the column.
Possible values are 'start', 'center', 'end'.

SPACING: A list specifying the Bootstrap spacing class. The list should contain
keyword arguments that can be passed to the ml-sbt/utility:spacing function.

Example 1:
  (col (:cols 6) \"Hello, world!\")
  ; This will generate a column that spans 6 columns by default, containing the
  ; text 'Hello, world!'.

Example 2:
  (col (:breakpoint (:kind \"col\" :md (8 2))) \"Hello, world!\")
  ; This will generate a column that spans 8 medium-sized columns with an
  ; offset of 2 medium-sized columns, containing the text 'Hello, world!'.

Example 3:
  (col (:alignself \"center\") \"Hello, world!\")
  ; This will generate a column that aligns its content in the center,
  ; containing the text 'Hello, world!'.

Example 4:
  (col (:spacing (:property \"p\" :size 2)) \"Hello, world!\")
  ; This will generate a column that has a padding of 2 units on all sides,
  ; containing the text 'Hello, world!'."
  (let ((class-str (string-clean
                    (concatenate 'string
                                 (if (null cols) "col " (format nil "col-~d " cols))
                                 (if (null breakpoint) ""
                                     (apply #'breakpoint-class breakpoint))
                                 (if (null alignself) "" (format nil "align-self-~a " alignself))
                                 (if (null spacing) ""
                                     (apply #'ml-sbt/utility:spacing spacing))))))
    `(spinneret:with-html
       (:div :class ,class-str
             ,@body))))

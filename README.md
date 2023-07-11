
# Table of Contents

1.  [Cl-Sbt](#org625f6f0)
    1.  [Usage](#org905a16a)
        1.  [Defining a Card](#orgca7c75b)
        2.  [Defining a Dropdown](#org9888757)
    2.  [Installation](#orgb5d14a1)
    3.  [Author](#org0e6a662)
    4.  [Copyright](#orge69a71c)


<a id="org625f6f0"></a>

# Cl-Sbt

Common Lisp Spinneret Bootstrap Templates


<a id="org905a16a"></a>

## Usage


<a id="orgca7c75b"></a>

### Defining a Card

The following code demonstrates how to define a card using the card macro.

    (cl-sbt-card:card
      (cl-sbt-card:body
        (cl-sbt-card:title "Card title")
        (cl-sbt-card:subtitle "Card subtitle")
        (cl-sbt-card:text "Some quick example text to build on the card title and make up the bulk of the card's content.")
        (cl-sbt-card:link (:href "#") "Card link")))


<a id="org9888757"></a>

### Defining a Dropdown

The following code demonstrates how to define a dropdown using the dropdown
macro.

    (cl-sbt-dropdown:dropdown (:title "Dropdown button")
      (cl-sbt-dropdown:menu
        (cl-sbt-dropdown:item "Action")
        (cl-sbt-dropdown:item "Another action")
        (cl-sbt-dropdown:item "Something else here")))


<a id="orgb5d14a1"></a>

## Installation

    git clone https://git.sr.ht/~marcuskammer/cl-sbt ~/quicklisp/local-projects/cl-sbt/

    (ql:quickload :cl-sbt)


<a id="org0e6a662"></a>

## Author

-   Marcus Kammer (marcus.kammer@mailbox.org)


<a id="orge69a71c"></a>

## Copyright

Copyright (c) 2023 Marcus Kammer (marcus.kammer@mailbox.org)

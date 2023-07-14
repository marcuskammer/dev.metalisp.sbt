
# Table of Contents

1.  [Cl-Sbt](#org6af45e7)
    1.  [Usage](#org4b1729c)
        1.  [Defining a Card](#org8a876fa)
        2.  [Defining a Dropdown](#orgcb10ca2)
    2.  [Installation](#org0da4cf2)
    3.  [Author](#org919d853)
    4.  [Copyright](#orgc51162e)


<a id="org6af45e7"></a>

# Cl-Sbt

A Common Lisp library for generating Bootstrap-based HTML markup. It provides
macros to easily create Bootstrap components such as accordions, alerts,
badges, buttons, cards, dropdowns, headers, list groups, navbars, nav-tabs,
pagination, and tables. This library is dependent on the Spinneret library for
HTML generation.


<a id="org4b1729c"></a>

## Usage


<a id="org8a876fa"></a>

### Defining a Card

The following code demonstrates how to define a card using the card macro.

    (cl-sbt-card:card
      (cl-sbt-card:body
        (cl-sbt-card:title "Card title")
        (cl-sbt-card:subtitle "Card subtitle")
        (cl-sbt-card:text "Some quick example text to build on the card title and make up the bulk of the card's content.")
        (cl-sbt-card:link (:href "#") "Card link")))

    <div class=card>
     <div class=card-body>
      <h5 class=card-title>Card title</h5>
      <h6
          class="card-subtitle mb-2 text-body-secondary">Card subtitle</h6>
      <p class=card-text>Some quick example text to build on the card title and make up
       the bulk of the card&#39;s content.
      <a class=card-link href=#>Card link</a>
     </div>
    </div>

The following code demonstrates how to define a card using the card-with-img macro.

    (cl-sbt-card:card-with-img (:img-src "test.jpg")
      (cl-sbt-card:title "Card title")
      (cl-sbt-card:subtitle "Card subtitle")
      (cl-sbt-card:text "Some quick example text to build on the card title and make up the bulk of the card's content.")
      (cl-sbt-card:link (:href "#") "Card link"))

    <div class=card>
     <img class=card-img-top src=test.jpg
          alt="Card Image">
     <div class=card-body>
      <h5 class=card-title>Card title</h5>
      <h6
          class="card-subtitle mb-2 text-body-secondary">Card subtitle</h6>
      <p class=card-text>Some quick example text to build on the card title and make up
       the bulk of the card&#39;s content.
      <a class=card-link href=#>Card link</a>
     </div>
    </div>


<a id="orgcb10ca2"></a>

### Defining a Dropdown

The following code demonstrates how to define a dropdown using the dropdown
macro.

    (cl-sbt-dropdown:dropdown (:title "Dropdown button")
      (cl-sbt-dropdown:menu
        (cl-sbt-dropdown:item "Action")
        (cl-sbt-dropdown:item "Another action")
        (cl-sbt-dropdown:item "Something else here")))

    <div class=dropdown>
     <button class="btn btn-secondary dropdown-toggle"
             type=button data-bs-toggle=dropdown
             aria-expanded=false>Dropdown button</button>
     <ul class=dropdown-menu>
      <li><a class=dropdown-item href=#>Action</a>
      <li><a class=dropdown-item href=#>Another action</a>
      <li><a class=dropdown-item href=#>Something else here</a>
     </ul>
    </div>


<a id="org0da4cf2"></a>

## Installation

    git clone https://git.sr.ht/~marcuskammer/cl-sbt ~/quicklisp/local-projects/cl-sbt/

    (ql:quickload :cl-sbt)


<a id="org919d853"></a>

## Author

-   Marcus Kammer (marcus.kammer@mailbox.org)


<a id="orgc51162e"></a>

## Copyright

Copyright (c) 2023 Marcus Kammer (marcus.kammer@mailbox.org)

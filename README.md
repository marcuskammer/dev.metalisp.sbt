

# Cl-Sbt

A Common Lisp library for generating Bootstrap-based HTML markup. It provides
macros to easily create Bootstrap components such as accordions, alerts,
badges, buttons, cards, dropdowns, headers, list groups, navbars, nav-tabs,
pagination, and tables. This library is dependent on the Spinneret library for
HTML generation.

The following components are available and are packages:

-   accordion
-   alert
-   badge
-   button
-   card
-   dropdown
-   list group
-   navbar
-   nav / tab
-   pagination
-   table
-   spinner


## Usage


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


## Installation

    git clone https://git.sr.ht/~marcuskammer/cl-sbt ~/quicklisp/local-projects/cl-sbt/

    (ql:quickload :cl-sbt)


## Author

-   Marcus Kammer (marcus.kammer@mailbox.org)


## Copyright

Copyright (c) 2023 Marcus Kammer (marcus.kammer@mailbox.org)

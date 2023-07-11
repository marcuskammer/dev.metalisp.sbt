
# Table of Contents

1.  [Cl-Sbt](#orge21ad75)
    1.  [Usage](#org8607bc4)
        1.  [Defining a Card](#org68a0ecb)
        2.  [Defining a Dropdown](#org01301e2)
    2.  [Installation](#orgad68576)
    3.  [Author](#org941d120)
    4.  [Copyright](#orgd118731)


<a id="orge21ad75"></a>

# Cl-Sbt

Common Lisp Spinneret Bootstrap Templates


<a id="org8607bc4"></a>

## Usage


<a id="org68a0ecb"></a>

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


<a id="org01301e2"></a>

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


<a id="orgad68576"></a>

## Installation

    git clone https://git.sr.ht/~marcuskammer/cl-sbt ~/quicklisp/local-projects/cl-sbt/

    (ql:quickload :cl-sbt)


<a id="org941d120"></a>

## Author

-   Marcus Kammer (marcus.kammer@mailbox.org)


<a id="orgd118731"></a>

## Copyright

Copyright (c) 2023 Marcus Kammer (marcus.kammer@mailbox.org)

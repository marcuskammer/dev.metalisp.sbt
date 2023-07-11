
# Table of Contents

1.  [Cl-Sbt](#orgf83023c)
    1.  [Usage](#org78733f2)
        1.  [Defining a Card](#org18999ea)
        2.  [Defining a Dropdown](#orgc4ef212)
    2.  [Installation](#org656f0ca)
    3.  [Author](#org1683a96)
    4.  [Copyright](#orgd092a88)


<a id="orgf83023c"></a>

# Cl-Sbt

Common Lisp Spinneret Bootstrap Templates


<a id="org78733f2"></a>

## Usage


<a id="org18999ea"></a>

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


<a id="orgc4ef212"></a>

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


<a id="org656f0ca"></a>

## Installation

    git clone https://git.sr.ht/~marcuskammer/cl-sbt ~/quicklisp/local-projects/cl-sbt/

    (ql:quickload :cl-sbt)


<a id="org1683a96"></a>

## Author

-   Marcus Kammer (marcus.kammer@mailbox.org)


<a id="orgd092a88"></a>

## Copyright

Copyright (c) 2023 Marcus Kammer (marcus.kammer@mailbox.org)

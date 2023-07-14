

# cl-sbt


## Introduction

A Common Lisp library for generating Bootstrap-based HTML markup. It provides
macros to easily create Bootstrap components such as accordions, alerts,
badges, buttons, cards, dropdowns, headers, list groups, navbars, nav-tabs,
pagination, and tables. This library is dependent on the Spinneret library for
HTML generation.

The following components are available as packages:

-   **accordion:** This package provides macros for generating Bootstrap Accordion
    components. Accordions are collapsible lists of items, each of which can be
    &ldquo;expanded&rdquo; or &ldquo;collapsed&rdquo; to reveal or hide content.

-   **alert:** This package offers macros for creating Bootstrap Alert components.
    Alerts are used to provide user feedback, usually in response to user
    interactions, such as form validation or actions.

-   **badge:** This package provides macros for creating Bootstrap Badge
    components. Badges are small status descriptors for UI elements. They can be
    used to add additional information to an element, such as a count.

-   **button:** This package provides macros for creating Bootstrap Button
    components. Buttons are used for actions, like submitting forms or initiating
    actions.

-   **card:** This package provides macros for creating Bootstrap Card components.
    Cards are flexible and extensible containers for displaying content in a
    structured format.

-   **dropdown:** This package provides macros for creating Bootstrap Dropdown
    components. Dropdowns are toggleable, contextual overlays for displaying
    lists of links and actions in a dropdown format.

-   **list group:** This package provides macros for generating Bootstrap List
    Group components. List groups are flexible and powerful components for
    displaying not only simple lists of elements, but complex ones with custom
    content.

-   **navbar:** This package provides macros for creating Bootstrap Navbar
    components. Navbars are responsive meta components that serve as navigation
    headers for your application or site.

-   **nav / tab:** This package provides macros for creating Bootstrap Nav
    components. Navs are navigation components that can be styled in different
    ways and can contain links, text, or any other kind of content.

-   **pagination:** This package provides macros for creating Bootstrap Pagination
    components. Pagination is used to let the user navigate through a set of
    discrete pages.

-   **table:** This package provides macros for generating Bootstrap Table
    components. Tables are used to present data in a tabular format.

-   **spinner:** This package provides macros for creating Bootstrap Spinner
    components. Spinners are used to indicate a loading state of a page or a
    section.


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

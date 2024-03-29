
# Table of Contents

1.  [dev.metalisp.sbt](#orgbb4b17d)
    1.  [Introduction](#org358d7a8)
    2.  [Installation](#orgd78b88c)
        1.  [Load library](#org60e3c78)
        2.  [Run tests](#orgc18a25a)
    3.  [Issue Tracker](#orga00e078)
    4.  [Mailing list](#orgef69d41)
    5.  [Similar Libraries](#org574412f)
    6.  [Author](#orgcf618ab)
    7.  [Copyright](#org128ba2a)


<a id="orgbb4b17d"></a>

# dev.metalisp.sbt


<a id="org358d7a8"></a>

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


<a id="orgd78b88c"></a>

## Installation

    git clone https://git.sr.ht/~marcuskammer/dev.metalisp.sbt ~/quicklisp/local-projects/dev.metalisp.sbt/


<a id="org60e3c78"></a>

### Load library

    (ql:quickload :dev.metalisp.sbt)


<a id="orgc18a25a"></a>

### Run tests

    cd ~/quicklisp/local-project/dev.metalisp.sbt/

    sh run-tests.sh


<a id="orga00e078"></a>

## Issue Tracker

-   <https://todo.sr.ht/~marcuskammer/dev.metalisp.sbt>


<a id="orgef69d41"></a>

## Mailing list

-   <https://lists.sr.ht/~marcuskammer/dev.metalisp.sbt>

-   <mailto:~marcuskammer/dev.metalisp.sbt@lists.sr.ht>


<a id="org574412f"></a>

## Similar Libraries

-   <https://github.com/rmhsilva/semantic-spinneret>

-   <https://github.com/thephoeron/cl-bootstrap>

-   <https://github.com/rajasegar/cl-bootstrap/>


<a id="orgcf618ab"></a>

## Author

-   Marcus Kammer (marcus.kammer@metalisp.dev)


<a id="org128ba2a"></a>

## Copyright

Copyright (c) 2023 Marcus Kammer (marcus.kammer@metalisp.dev)


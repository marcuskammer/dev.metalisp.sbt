
# Table of Contents

1.  [dev.metalisp.sbt](#org8bf2b0f)
    1.  [Introduction](#org9064803)
    2.  [Installation](#orgae5e4dd)
    3.  [Issue Tracker](#org0000d2d)
    4.  [Mailing list](#orgb384f3b)
    5.  [Similar Libraries](#orga333aa3)
    6.  [Author](#orgd1478d1)
    7.  [Copyright](#org9ae3ef0)


<a id="org8bf2b0f"></a>

# dev.metalisp.sbt


<a id="org9064803"></a>

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


<a id="orgae5e4dd"></a>

## Installation

    git clone https://git.sr.ht/~marcuskammer/dev.metalisp.sbt ~/quicklisp/local-projects/dev.metalisp.sbt/

    (ql:quickload :dev.metalisp.sbt)


<a id="org0000d2d"></a>

## Issue Tracker

-   <https://todo.sr.ht/~marcuskammer/dev.metalisp.sbt>


<a id="orgb384f3b"></a>

## Mailing list

-   <https://lists.sr.ht/~marcuskammer/dev.metalisp.sbt>

-   <mailto:~marcuskammer/dev.metalisp.sbt@lists.sr.ht>


<a id="orga333aa3"></a>

## Similar Libraries

-   <https://github.com/rmhsilva/semantic-spinneret>

-   <https://github.com/thephoeron/cl-bootstrap>

-   <https://github.com/rajasegar/cl-bootstrap/>


<a id="orgd1478d1"></a>

## Author

-   Marcus Kammer (marcus.kammer@metalisp.dev)


<a id="org9ae3ef0"></a>

## Copyright

Copyright (c) 2023 Marcus Kammer (marcus.kammer@metalisp.dev)


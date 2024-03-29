
# Table of Contents

1.  [dev.metalisp.sbt](#org61f4e75)
    1.  [Introduction](#orgd236391)
    2.  [Installation](#orgcd799cd)
        1.  [Load library](#orgdc6bab8)
        2.  [Run tests](#org3ff6c1d)
    3.  [Issue Tracker](#org082fbec)
    4.  [Mailing list](#org4864ea5)
    5.  [RSS Feed](#orgd23014e)
    6.  [Similar Libraries](#org1f515f8)
    7.  [Author](#org6e48505)
    8.  [Copyright](#org396e36e)


<a id="org61f4e75"></a>

# dev.metalisp.sbt


<a id="orgd236391"></a>

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


<a id="orgcd799cd"></a>

## Installation

    git clone https://git.sr.ht/~marcuskammer/dev.metalisp.sbt ~/quicklisp/local-projects/dev.metalisp.sbt/


<a id="orgdc6bab8"></a>

### Load library

    (ql:quickload :dev.metalisp.sbt)


<a id="org3ff6c1d"></a>

### Run tests

    cd ~/quicklisp/local-projects/dev.metalisp.sbt/

    sh run-tests.sh


<a id="org082fbec"></a>

## Issue Tracker

-   <https://todo.sr.ht/~marcuskammer/dev.metalisp.sbt>


<a id="org4864ea5"></a>

## Mailing list

-   <https://lists.sr.ht/~marcuskammer/dev.metalisp.sbt>

-   <mailto:~marcuskammer/dev.metalisp.sbt@lists.sr.ht>


<a id="orgd23014e"></a>

## RSS Feed

-   <https://git.sr.ht/~marcuskammer/dev.metalisp.sbt/log/main/rss.xml>


<a id="org1f515f8"></a>

## Similar Libraries

-   <https://github.com/rmhsilva/semantic-spinneret>

-   <https://github.com/thephoeron/cl-bootstrap>

-   <https://github.com/rajasegar/cl-bootstrap/>


<a id="org6e48505"></a>

## Author

-   Marcus Kammer (marcus.kammer@metalisp.dev)


<a id="org396e36e"></a>

## Copyright

Copyright (c) 2023 Marcus Kammer (marcus.kammer@metalisp.dev)


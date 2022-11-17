
<!-- README.md is generated from README.Rmd. Please edit that file -->

# music

<!-- badges: start -->
<!-- badges: end -->

The goal of music has two sides:

-   explore music theory through opinionated declarative functions;
-   explore R programming and demonstrate some development skills.

See the [Extra](#extra) section for a more detailed description.

## Installation

You can install the development version of music like so:

``` r
#install.packages("pak")
pak::pkg_install("c-marc/music")
```

## Example

1.  Start with notes C, F and A, and assume root note is D.
2.  Add the role each of these note play.
3.  Transform the index so that it now relates to half-tones.
4.  Show the first *inversion* of that chord (starting on its second
    note).
5.  Compute intervals.

``` r
library(music)
#> 
#> Attaching package: 'music'
#> The following object is masked from 'package:utils':
#> 
#>     example

start_with_some_notes(c("C", "F", "A"), root_note = "D") |>
  add_roles() |>
  set_unit_to_1() |>
  set_first(rank = 2) |>
  dplyr::mutate(intervals = c(0, diff(idx)))
#> # A tibble: 3 × 4
#>            idx note  role  intervals
#>   <i<(0)1/12>> <chr> <chr>     <dbl>
#> 1           -5 A     P5            0
#> 2           -2 C     m7            3
#> 3            3 F     m3            5
```

This is (D)-7/A: rootless D minor 7 played over A…

## Extra

### Music theory

This is an interesting subject to play with for the following reasons:

-   It involves a meaningful subject to play with, and one can actually
    learn some things while playing with data and functions. Yet no
    specific knowledge is required here, and the implementation is
    simple enough to be used in an abstract way.

-   In the scope of this package, it involves two supplementary
    interesting features for exploration learning and/or teaching
    programming skills:

    1.  It relies on **simple blocks**. Each of these blocks is quite
        small (generally up to 12 rows and 1-3 columns). Each of these
        block can be used on its own or combined with the others.

    2.  It involves **strong logic**. That means there’s a lot to
        experiment with, from transforming to joining these blocks. Or
        rebuilding some just by using the others.

### Programming

#### General goals

This package has the following general goals:

-   to provide a framework with declarative and pure functions that can
    be used intuitively with basic *tidyverse* skills
-   to provide a carefully crafted package:
    -   strong checking of users inputs, and informative messaging;
    -   decent documentation;
    -   nice and organised coding.

#### Programming topics or skills

It implements:

-   OOP in S3 framework, with subclasses of `vctrs` and `tibbles`;
-   pretty interactions with `cli`;
-   some features requiring programming with *tidy-selection*,
    *data-masking*, and *glue injections*;
-   recursive functions;
-   dangerous arithmetic (especially for non interactive use);
-   unit testing, and safe programming by being quite strict with types,
    objects and arguments checking…

#### Design choices

Main design choices are:

##### For objects:

-   implement a powerful index class that can deal with periodicity and
    be transformed;
-   implement a subclass of a tibble with that index as a column;

##### For functions

We define 3 levels:

1.  Low level functions are internal…
2.  Mild level functions are in the NAMESPACE: they make available some
    operations to the user. They require some knowledge to be called
    meaningfully. And they usually must be sequenced with each other,
    with joining, and arranging… They require to explicitly unlock the
    data.
3.  Higher-order functions should work without much thinking from the
    user and show strong stability over time, hiding potential
    underlying changes. They can operate on locked data.

##### For data

We experiment an idea of data *as functions*. This is very simple in
that it just wraps data objects behind a proxy function. Our motivations
for such a choice are that:

-   this *shielding* of data behind function brings safety for
    interactive use. Even though our functions are pure, this encourages
    the user to call reproducible data (blocks). It’s just an assignment
    away. But as most problems should be solvable in just a few lines,
    these original blocks do not need to be tempered with. An
    intermediary assignment will sometimes makes a solution clearer, but
    most piping sequence are encouraged to start with these blocks.
-   this blurs the distinction between hard coded data and some blocks
    that can be built on the fly. Something we want to take advantage
    of. This allows extensions without actually adding data: other
    *blocks* can be proposed as arguments through limited getting
    functions; these can be a view of a block, pre-assembled blocks,
    modified blocks, etc… This should fit with our approach of using a
    limited set of clear and intuitive verbs. This functional approach
    wraps the OOP parts (it actually promotes the top level methods of
    the richest objects).

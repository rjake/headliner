
<!-- README.md is generated from README.Rmd. Please edit that file -->

# headliner <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/headliner)](https://cran.r-project.org/package=headliner)
[![R-CMD-check](https://github.com/rjake/headliner/workflows/R-CMD-check/badge.svg)](https://github.com/rjake/headliner/actions)
[![codecov](https://codecov.io/gh/rjake/headliner/branch/main/graph/badge.svg?token=2OZ6INQBYA)](https://codecov.io/gh/rjake/headliner)
<!-- [![CRAN Downloads](https://cranlogs.r-pkg.org/badges/grand-total/headliner)](https://cran.r-project.org/package=headliner) -->
<!-- badges: end -->

The goal of `headliner` is to translate facts into insights. Given two
values, `headliner` generates building blocks for creating dynamic text.
These talking points can be combined using using `glue` syntax to add
informative titles to plots, section headers or other text in a report.

## Installation

You can install the dev version of `headliner` from
[github](https://github.com/rjake/headliner) with:

``` r
devtools::install_github("rjake/headliner")
```

Let’s look at some of the talking points for the difference between 5
and 7:

``` r
library(headliner)

compare_values(5, 7) |> # returns a list
  view_list() # show as a data frame
```

    ##                       VALUES
    ## x                          5
    ## y                          7
    ## delta                      2
    ## delta_p                 28.6
    ## article_delta            a 2
    ## article_delta_p       a 28.6
    ## raw_delta                 -2
    ## raw_delta_p            -28.6
    ## article_raw_delta       a -2
    ## article_raw_delta_p  a -28.6
    ## sign                      -1
    ## orig_values          5 vs. 7
    ## trend               decrease

We can string these together these talking points like this:

``` r
headline(
  x = 5, 
  y = 7, 
  headline = "There was {article_delta_p}% {trend} ({orig_values})"
)
```

    ## [1] "There was a 28.6% decrease (5 vs. 7)"

See [here](https://rjake.github.io/headliner/articles/intro.html) for a
longer introduction.

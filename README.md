
<!-- README.md is generated from README.Rmd. Please edit that file -->

# headliner

<!-- badges: start -->

<!-- badges: end -->

The goal of `headliner` is to help analysts to translate facts to
insights. In the comparison below
([source](https://blog.prototypr.io/dashboard-design-5-things-every-ux-designer-should-know-a85c4558d75)),
both dashboards have the same underlying data but how they present the
information to the user is very different.

<div style="text-align:center">

<img src="man/figures/facts_vs_insights.png"  width=600/>

</div>

Right now, there isn’t anything out of the box to help users dynamically
create phrasing like used in the “insights” version without a bit of
coding gymnastics. The many ways you could approach it combined with the
steps required to say “if positive, show it like this, if negative show
it like that” increase the technical debt this kind of code could add to
a project. For this reason, `headliner` is designed to deliver the
building blocks required to create these phrases for plot titles, value
boxes in `shiny` or section headers in a report.

## Installation

You can install the dev version of `headliner` from
[github](https://github.com/rjake/headliner) with:

``` r
devtools::install_github("rjake/headliner")
```

## Usage

For these examples, I will use a function called `demo_data()` to build
a data set based on the current date 09/07/20.

``` r
library(headliner)
library(glue)

demo_data()
```

    #> # A tibble: 10 x 5
    #>    group     x     y     z date      
    #>    <chr> <dbl> <dbl> <dbl> <date>    
    #>  1 a       101    10     1 2020-09-07
    #>  2 a       102    20     0 2020-07-09
    #>  3 b       103    30     1 2020-05-10
    #>  4 b       104    40     0 2020-03-11
    #>  5 c       105    50     1 2020-01-11
    #>  6 c       106    60     0 2019-11-12
    #>  7 d       107    70     1 2019-09-13
    #>  8 d       108    80     0 2019-07-15
    #>  9 e       109    90     1 2019-05-16
    #> 10 e       110   100     0 2019-03-17

What we want is to say something like this:

    #> We have seen a 5.6% decrease compared to the same time last year.

We can look at the data an see that about 12 months ago, x was 107 where
as today it is 101. One function available in headliner is
`compare_values()`. This function returns a named list.

``` r
compare_values(101, 107, calc = "prop") %>% head(2)
#> $delta
#> [1] 5.6
#> 
#> $trend
#> [1] "decrease"
```

With this list, you can use `glue_data()` to combine the results.

``` r
compare_values(101, 107, calc = "prop") %>% 
  glue_data(
    "we had a {delta}% {trend} since last year ({expr})"
  )
#> we had a 5.6% decrease since last year (101 vs. 107)
```

But let’s see if we can make this more dynamic…

First, we can use a function called `add_date_columns()` to calculate
distances from the current date (or the refence date specified) to the
values in the `date` column . With these new fields we can see that
07/09/20 was 60 days ago (or 9 weeks or 2 months, …) from the current
date.

``` r
demo_data() %>%
  add_date_columns(date_col = date)
#> # A tibble: 10 x 11
#>    group     x     y     z date         day  week month quarter calendar_year
#>    <chr> <dbl> <dbl> <dbl> <date>     <dbl> <dbl> <dbl>   <dbl>         <dbl>
#>  1 a       101    10     1 2020-09-07     0     0     0       0             0
#>  2 a       102    20     0 2020-07-09   -60    -9    -2       0             0
#>  3 b       103    30     1 2020-05-10  -120   -18    -4      -1             0
#>  4 b       104    40     0 2020-03-11  -180   -26    -6      -2             0
#>  5 c       105    50     1 2020-01-11  -240   -35    -8      -2             0
#>  6 c       106    60     0 2019-11-12  -300   -43   -10      -3            -1
#>  7 d       107    70     1 2019-09-13  -360   -52   -12      -4            -1
#>  8 d       108    80     0 2019-07-15  -420   -60   -14      -4            -1
#>  9 e       109    90     1 2019-05-16  -480   -69   -16      -5            -1
#> 10 e       110   100     0 2019-03-17  -540   -78   -18      -6            -1
#> # ... with 1 more variable: fiscal_year <dbl>
```

We can then identify some conditions for our comparison (`compare`) and
our reference group (`reference`). This step uses the kind of logic you
would use in `dplyr::filter()` or `base::subset()`

``` r
demo_data() %>%
  add_date_columns(date) %>% 
  compare_conditions(
    compare = (month == 0),     # this month
    reference = (month == -12), # vs 12 months ago
    cols = c(x),                # the column(s) to aggregate
    calc = list(mean = mean)    # the list of functions passed to summarise(across(...))
  )
#> $mean_x_comp
#> [1] 101
#> 
#> $mean_x_ref
#> [1] 107
```

It might look funny to see `list(mean = mean)`. The name (left side) is
how it will name the values, the right side is the function to use. If I
had used `calc = list(avg = mean)` The names would have been `avg_x_*`.
Because `compare_conditions()` uses the mean as the default, I’ll omit
it going forward. Now that I have my output as a named list, I can pipe
it into `compare_values()`. This step will give me the building blocks
to write my phrases.

``` r
compare <-
  demo_data() %>%
  add_date_columns(date) %>% 
  compare_conditions(
    compare = (month == 0),
    reference = (month == -12),
    cols = c(x)
  ) %>% 
  compare_values(
    compare = mean_x_comp, 
    reference = mean_x_ref,
    calc = "prop"
  )

compare
#> $delta
#> [1] 5.6
#> 
#> $trend
#> [1] "decrease"
#> 
#> $comp_value
#> [1] 101
#> 
#> $ref_value
#> [1] 107
#> 
#> $raw_delta
#> [1] -5.6
#> 
#> $sign
#> [1] -1
#> 
#> $calc
#> [1] "prop"
#> 
#> $expr
#> 101 vs. 107
```

I can then use `paste()` or `glue::glue_data()` to create my final
phrase

``` r
compare %>% 
  glue_data(
    "We have seen a {delta}% {trend} compared to the same time last year ({expr})"
  )
#> We have seen a 5.6% decrease compared to the same time last year (101 vs. 107)
```

We can also use these pieces in a `valueBox()`

``` r
box_color <- ifelse(compare$sign == -1, "red", "blue")

shinydashboard::valueBox(
  value = glue_data(compare, '{delta}% {trend}'),
  subtitle = "vs. the same time last year",
  color = box_color
) 
```

<div style="text-align:center">

<img src="man/figures/value_box.png"  width=300/>

</div>

We can also used `compare_conditions()` to compare categorical criteria.
Here I am using `trend_terms()` to use “more” and “less” instead of the
default “increase” and “decrease” terminology.

``` r
demo_data() %>%
  compare_conditions(
    compare = group == "a",
    reference = group == "c",
    cols = c(x)
  ) %>% 
  compare_values(
    compare = mean_x_comp, 
    reference = mean_x_ref, 
    trend_phrasing = 
      trend_terms(more = "up",  less = "down")
  ) %>% 
  glue_data(
    "Group A is {trend} {delta} points ({comp_value}) \\
    compared to Group C ({ref_value})"
  )
#> Group A is down 4 points (101.5) compared to Group C (105.5)
```

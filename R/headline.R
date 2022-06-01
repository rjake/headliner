#' Compose phrases that describe differences in the data
#'
#' @description Given two values, `headline()` will use
#' \code{\link[glue]{glue}} syntax to string together "talking points".
#' For example `headline(8, 10)` will describe a difference of 2 and can be
#' expressed as
#' `headline(8, 10, headline = "changed by {delta} ({raw_delta_p}%)")`. This
#' returns "changed by 2 (-20%)".
#'
#' @return Returns a character vector the same length as the input,
#'
#' @details `headline()` relies heavily on \code{\link[glue]{glue_data}}.
#' Objects can be combined into a headline using the following search path:
#' If given
#' ```{r}
#' delta <- 123
#' headline(1, 3, delta = "xxxxxx")
#' ```
#' `delta` is one of the "talking points" from `compare_values()` and would
#' usually return "2" but because we passed the named variable
#' `delta = "none"`, `headline()` (really  \code{\link[glue]{glue_data}})
#' will look first at the named variables, then at the result of
#' `compare_values()` then in the global environment. So in the example above,
#' the output will return `"decrease of xxxxxx (1 vs. 3)"`
#'
#' @param headline a string to format the final output. Uses
#' \code{\link[glue]{glue}} syntax
#' @param ... arguments passed to \code{\link[glue]{glue_data}}
#' @param if_match string to display if numbers match, uses
#' \code{\link[glue]{glue}} syntax
#' @param plural_phrases named list of values to use when difference (delta) is
#' singular (delta = 1) or plural (delta != 1)
#' @param n_decimal numeric value to limit the number of decimal places in
#' the returned values.
#' @param round_all logical value to indicate if all values should be rounded.
#' When FALSE, the values will return with no modification. When TRUE (default)
#' all values will be round to the length specified by 'n_decimal'.
#' @param multiplier number indicating the scaling factor. When multiplier = 1
#' (default), 0.25 will return 0.25. When multiplier = 100, 0.25 will return 25.
#' @param return_data logical to indicate whether function should return the
#' talking points used to compose the headline
#' @inheritParams compare_values
#' @importFrom glue glue_data as_glue
#' @importFrom purrr  map2_chr map_dbl map2
#' @export
#' @rdname headline
#' @seealso [compare_values()], [trend_terms()], and [add_article()]
#' @examples
#' # values can be manually entered, some headlines are provided by default
#' headline(10, 8)
#' headline(8, 10)
#' headline(1:3, 3:1)
#'
#' # most likely you'll edit the headline by hand
#' headline(
#'   x = 10,
#'   y = 8,
#'   headline = "There was a ${delta} {trend} vs last year"
#' )
#'
#' # you can also adjust the phrasing of higher/lower values
#' headline(
#'   x = 10,
#'   y = 8,
#'   headline = "Group A was {trend} by {delta_p}%.",
#'   trend_phrases = trend_terms(more = "higher", less = "lower")
#'  )
#'
#' # a phrase about the comparion can be edited by providing glue syntax
#' # 'c' = the 'compare' value, 'r' = 'reference'
#' headline(10, 8, orig_values = "{x} to {y} people")
#'
#' # you can also add phrases for when the difference = 1 or not
#' headline(
#'   x = 10,
#'   y = 8,
#'   plural_phrases = list(
#'     were = plural_phrasing(single = "was", multi = "were"),
#'     people = plural_phrasing(single = "person", multi = "people")
#'   ),
#'   headline = "there {were} {delta} {people}"
#' )
#'
#' # you can also adjust the rounding, the default is 1
#' headline(0.1234, 0.4321)
#' headline(0.1234, 0.4321, n_decimal = 3)
#' # or use a multiplier
#' headline(0.1234, 0.4321, multiplier = 100)
#'
#' # there are many components you can assemble
#' headline(
#'   x = 16,
#'   y = 8,
#'   headline = "there was {article_delta_p}% {trend}, \\
#'   {add_article(trend)} of {delta} ({orig_values})"
#' )
#'
headline <- function(x,
                     y,
                     headline = "{trend} of {delta} ({orig_values})",
                     ...,
                     if_match = "There was {trend}",
                     trend_phrases = headliner::trend_terms(),
                     plural_phrases = NULL,
                     orig_values = "{x} vs. {y}",
                     n_decimal = 1,
                     round_all = TRUE,
                     multiplier = 1,
                     return_data = FALSE) {
  res <-
    map2(
      .x = x,
      .y = y,
      .f =
        ~compare_values(
          .x,
          .y,
          trend_phrases = trend_phrases,
          plural_phrases = plural_phrases,
          orig_values = orig_values,
          n_decimal = n_decimal,
          round_all = round_all,
          multiplier = multiplier,
          check_rounding = FALSE # will do separately to limit # of warnings
        )
    )

  # check rounding
  check_rounding(x, y, n_decimal)

  # determine which headline phrasing to use & pass to glue
  headlines <-
    map2_chr(
      .x = res,
      .y = ifelse(map_dbl(res, pluck, "sign") == 0, if_match, headline),
      .f = glue_data,
      ...
    )


  if (return_data) {
    full_list <- map2(res, headlines, ~append(list(headline = .y), .x))
    return(full_list)
  }

  as_glue(headlines)
}


#' @param l a list with values to compare, if named, can call by name
#' @inheritParams headline
#' @rdname headline
#' @export
#' @examples
#'
#' # compare_conditions() produces a one-row data frame that can be
#' # passed to headline_list()
#' pixar_films |>
#'   compare_conditions(
#'     x = (rating == "G"),
#'     y = (rating == "PG"),
#'     rotten_tomatoes
#'   ) |>
#'   headline_list(
#'     headline = "On average, G-rated films score {delta} points {trend} than \\
#'     PG films on Rotten Tomatoes",
#'     trend_phrases = trend_terms(more = "higher", less = "lower")
#'   )
#'
#' # if you have more than 2 list items, you can specify them by name
#' list(
#'   x = 1,
#'   y = 2,
#'   z = 3
#'  ) |>
#'   headline_list(
#'     x = x,
#'     y = z
#'   )
headline_list <- function(l,
                          headline = "{trend} of {delta} ({orig_values})",
                          x,
                          y,
                          ...,
                          if_match = "There was no difference.",
                          trend_phrases = headliner::trend_terms(),
                          plural_phrases = NULL,
                          orig_values = "{x} vs. {y}",
                          n_decimal = 1,
                          round_all = TRUE,
                          multiplier = 1,
                          return_data = FALSE) {
  if (missing(x) & missing(y)) {
    if (length(l) > 2) {
      message(
        "Only the first two elements were used. ",
        "To specify values, pass a list of only two elements long or ",
        "specify using 'x' and 'y'"
      )
    }
    comp <- l[[1]][1]
    ref <- l[[2]][1]
  } else {
    comp <- l[[deparse(match.call()[["x"]])]]
    ref <- l[[deparse(match.call()[["y"]])]]
  }

  headline(
    x = comp,
    y = ref,
    headline = headline,
    ...,
    if_match = if_match,
    trend_phrases = trend_phrases,
    orig_values = orig_values,
    n_decimal = n_decimal,
    round_all = round_all,
    multiplier = multiplier,
    return_data = return_data
  )
}

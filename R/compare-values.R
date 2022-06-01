#' Compare two values and get talking points
#'
#' @description A function to create "talking points" that
#' describes the difference between two values.
#'
#' @return `compare_values()` returns a list object that can be used with
#' \code{\link[glue]{glue}} syntax
#'
#' @details
#' Given `compare_values(x = 8, y = 10)` the following items will be returned
#' in the list:
#'
#' |item      | value   | description                         |
#' |---       |---      |---                                  |
#' |`x`       | 2 | original `x` value to compare against `y` |
#' |`y`       | 10| original `y` value                        |
#' |`delta`   | 8 | absolute difference between `x` & `y`     |
#' |`delta_p` | 80| % difference between `x` & `y`            |
#' |`article_delta`   | "an 8" | `delta` with the article included   |
#' |`article_delta_p` | "an 80"| `delta_p` with the article included |
#' |`raw_delta`       |  -8| true difference between `x` & `y`   |
#' |`raw_delta_p`     | -80| true % difference between `x` & `y` |
#' |`article_raw_delta`   | "a -8" | `raw_delta` with the article   |
#' |`article_raw_delta_p` | "a -80"| `raw_delta_p` with the article |
#' |`sign` | -1 | the direction, 1 (increase), -1 (decrease), or 0 (no change) |
#' |`orig_values` | "2 vs. 10"| shorthand for `{x} vs {y}` |
#' |`trend` | "decrease"| influenced by the values in `trend_phrases` argument |
#'
#'
#' @param x a numeric value to compare to the reference value of 'y'
#' @param y a numeric value to act as a control for the 'x' value
#' @param trend_phrases list of values to use for when y is more than x, y is the
#' same as x, or y is less than x. You can pass it just
#' \code{\link{trend_terms}} (the default) and call the result with
#' \code{"...{trend}..."} or pass is a named list (see examples)
#' @param plural_phrases named list of values to use when difference (delta) is
#' singular (delta = 1) or plural (delta != 1)
#' @param orig_values a string using \code{\link[glue]{glue}} syntax.
#' example: `({x} vs {y})`
#' @param n_decimal numeric value to limit the number of decimal places in
#' the returned values.
#' @param round_all logical value to indicate if all values should be rounded.
#' When FALSE, the values will return with no modification. When TRUE (default)
#' all values will be round to the length specified by 'n_decimal'.
#' @param multiplier number indicating the scaling factor. When multiplier = 1
#' (default), 0.25 will return 0.25. When multiplier = 100, 0.25 will return 25.
#' @param check_rounding when TRUE (default) inputs will be checked to confirm if
#' a difference of zero may be due to rounding. Ex: 0.16 and 0.24 with
#' 'n_decimal = 1' will both return 0.2. Because this will show no difference,
#' a message will be displayed
#' @importFrom glue glue
#' @importFrom purrr map_if
#' @export
#' @md
#' @rdname compare_values
#' @seealso [headline()], [trend_terms()], [plural_phrasing()] and [view_list()]
#' @examples
#' # the values can be manually entered
#'
#' compare_values(10, 8) |> head(2)
#' # percent difference (10-8)/8
#' compare_values(10, 8)$delta_p
#'
#' # trend_phrases returns an object called trend if nothing is passed
#' compare_values(10, 8)$trend
#'
#' # or if one argument is passed using trend_terms()
#' compare_values(10, 8, trend_phrases = trend_terms(more = "higher"))$trend
#'
#' # if a named list is used, the objects are called by their names
#' compare_values(
#'   10, 8,
#'   trend_phrases = list(
#'     more = trend_terms(),
#'     higher = trend_terms("higher", "lower")
#'   )
#' )$higher
#'
#' # a phrase about the comparison can be edited by providing glue syntax
#' # 'c' = the 'compare' value, 'r' = 'reference'
#' compare_values(10, 8, orig_values = "{x} to {y} people")$orig_values
#'
#' # you can also adjust the rounding, although the default is 1
#' compare_values(0.1234, 0.4321)$orig_values
#' compare_values(0.1234, 0.4321, n_decimal = 3)$orig_values
#' # or add a multiplier
#' compare_values(0.1234, 0.4321, multiplier = 100)$orig_values
compare_values <- function(x,
                           y,
                           trend_phrases = headliner::trend_terms(),
                           orig_values = "{x} vs. {y}",
                           plural_phrases = NULL,
                           n_decimal = 1,
                           round_all = TRUE,
                           multiplier = 1,
                           check_rounding = TRUE) {
  # calcs
  comp <- (x * multiplier)
  ref <- (y * multiplier)

  delta <- as.numeric(comp - ref)
  delta_p <- as.numeric(delta / ref  * 100)

  calc <-
    list(
      delta = delta,
      delta_p = delta_p,
      sign = sign(delta),
      abs_delta = abs(delta),
      abs_delta_p = abs(delta_p),
      x = comp,
      y = ref
    )

  if (round_all) {
    # give a warning if rounding causes a delta of 0 due to inputs having
    # decimals >= n_decimal parameter
    if (check_rounding){
      check_rounding(
        x = calc$x,
        y = calc$y,
        n_decimal = n_decimal
      )
    }

    calc <-
      calc |>
      map_if(is.numeric, round, n_decimal)
  }


  trend_terms_list <- return_trend_phrases(trend_phrases, calc$sign)

  output <-
    list(
      x = calc$x,
      y = calc$y,
      delta = calc$abs_delta,
      delta_p = calc$abs_delta_p,
      article_delta = paste(get_article(calc$abs_delta), calc$abs_delta),
      article_delta_p = paste(get_article(calc$abs_delta_p), calc$abs_delta_p),
      raw_delta = calc$delta,
      raw_delta_p = calc$delta_p,
      article_raw_delta = paste(get_article(calc$delta), calc$delta),
      article_raw_delta_p = paste(get_article(calc$delta_p), calc$delta_p),
      sign = calc$sign,
      orig_values = glue(
        orig_values,
        x = calc$x,
        y = calc$y
      )
    )

  # apppend trend terms
  output <- append(output, trend_terms_list)

  # append plural phrases if provided
  if (!is.null(plural_phrases)) {
    plural_phrases_list <-
      return_plural_phrases(plural_phrases, delta = calc$abs_delta)

    # append to list
    output <- append(output, plural_phrases_list)
  }

  output
}



#' identify trend terms to use
#' @noRd
#' @importFrom purrr map pluck
#' @examples
#' return_trend_phrases(trend_terms())
#' return_trend_phrases(list(trend = trend_terms()))
#' return_trend_phrases(
#'   list(trend = trend_terms(), x = trend_terms(), y = trend_terms())
#' )
return_trend_phrases <- function(trend_phrases, sign = 1) {
  # trend_terms() returns a list, this test is:
  # FALSE when passed alone
  # TRUE when used in a list of trend terms
  passed_as_list <- is.list(trend_phrases[[1]])

  if(passed_as_list) {
    trend_list <- trend_phrases
  } else {
    # else make list
    trend_list <- list(trend = trend_phrases)
  }

  which_trend <-
    switch(
      as.character(sign),
      "1" = "more",
      "-1" = "less",
      "0" = "same"
    )

  map(trend_list, pluck, which_trend)
}


#' identify plural phrases to use
#' @noRd
#' @importFrom purrr map pluck
#' @examples
#' return_plural_phrases(
#'   plural_phrases = list(x = plural_phrasing("one", "many"))
#' )
#' return_plural_phrases(
#'   list(
#'     x = plural_phrasing("one", "many"),
#'     y = plural_phrasing("one", "many")
#'   )
#' )
return_plural_phrases <- function(plural_phrases, delta = 2) {
  # stop if items in list aren't named
  if (any(is.null(names(plural_phrases)))) {
    stop(
      "'plural_phrases' should be a named list. \nex. ",
      'list(people = plural_phrasing("person", "people"))',
      call. = FALSE
    )
  }

  # identify which list element to choose if delta = 1 then single else multi
  value <- ifelse(delta == 1, "single", "multi")

  # select correct value
  map(plural_phrases, pluck, value)
}


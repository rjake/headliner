#' Compose phrases that describe differences in the data
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
#' phrase components used to compose the headline
#' @inheritParams compare_values
#' @importFrom glue glue_data
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
                     if_match = "There was no difference.",
                     trend_phrases = headliner::trend_terms(),
                     plural_phrases = NULL,
                     orig_values = "{x} vs. {y}",
                     n_decimal = 1,
                     round_all = TRUE,
                     multiplier = 1,
                     return_data = FALSE) {
    headline <- headliner_global$headline
  }

  if (missing(orig_values)) {
    orig_values <- headliner_global$orig_values
  }

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

  headlines
}


#' @param l a list with values to compare, if named, can call by name
#' @inheritParams headline
#' @rdname headline
#' @export
#' @examples
#'
#' # compare_conditions() produces a list that can be
#' # passed to headline_list()
#' flights_jfk |>
#'   compare_conditions(
#'     x = (hour > 12),
#'     y = (hour <= 12),
#'     dep_delay
#'   ) |>
#'   headline_list()
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
      stop(paste(
        "Not sure which columns to use, please pass list of two",
        "elements long or specify using 'x' and 'y'"
      ), call. = FALSE)
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

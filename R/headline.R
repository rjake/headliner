#' Compose phrases that describe differences in the data
#' @param compare a numeric value to compare to a reference value
#' @param reference a numeric value to act as a control for the 'compare' value
#' @param headline a string to format the final output. Uses
#' \code{\link[glue]{glue}} syntax
#' @param ... arguments passed to \code{\link[glue]{glue_data}}
#' @param if_match string to display if numbers match, uses
#' \code{\link[glue]{glue}} syntax
#' @param plural_phrases named list of values to use when difference (delta) is
#' singular (delta = 1) or plural (delta != 1)
#' @param orig_values a string to display the two original values. Uses
#'  \code{\link[glue]{glue}} syntax. `{c}` = the 'compare' value, and
#'  `{r}` = 'reference'
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
#' @importFrom purrr map_if
#' @export
#' @rdname headline
#' @seealso [compare_values()], [trend_terms()], and [add_article()]
#' @examples
#' # values can be manually entered, some headlines are provided by default
#' headline(10, 8)
#' headline(8, 10)
#' headline(10, 10)
#'
#' # most likely you'll edit the headline by hand
#' headline(
#'   compare = 10,
#'   reference = 8,
#'   headline = "There was a ${delta} {trend} vs last year"
#' )
#'
#' # you can also adjust the phrasing of higher/lower values
#' headline(
#'   compare = 10,
#'   reference = 8,
#'   headline = "Group A was {trend} by {delta_p}%.",
#'   trend_phrases = trend_terms(more = "higher", less = "lower")
#'  )
#'
#' # a phrase about the comparion can be edited by providing glue syntax
#' # 'c' = the 'compare' value, 'r' = 'reference'
#' headline(10, 8, orig_values = "{c} to {r} people")
#'
#' # you can also add phrases for when the difference = 1 or not
#' headline(
#'   compare = 10,
#'   reference = 8,
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
#'   compare = 16,
#'   reference = 8,
#'   headline = "there was {article_delta_p}% {trend}, \\
#'   {add_article(trend)} of {delta} ({orig_values})"
#' )
#'
headline <- function(compare,
                     reference,
                     headline = "{trend} of {delta} ({orig_values})",
                     ...,
                     if_match = "There was no difference.",
                     trend_phrases = headliner::trend_terms(),
                     plural_phrases = NULL,
                     orig_values = "{c} vs. {r}",
                     n_decimal = 1,
                     round_all = TRUE,
                     multiplier = 1,
                     return_data = FALSE) {
  res <-
    compare_values(
      compare,
      reference,
      trend_phrases = trend_phrases,
      plural_phrases = plural_phrases,
      orig_values = orig_values,
      n_decimal = n_decimal,
      round_all = round_all,
      multiplier = multiplier
    )

  final_output <- glue_data(res, headline, ...)

  if (return_data) {
    res <- append(res, list(headline = final_output))
    return(res)
  }

  # determine which headline phrasing to use
  final_output[res$sign == 0] <- glue_data(res, if_match, ...)

  final_output
}


#' @param x a list with values to compare, if named, can call by name
#' @inheritParams headline
#' @rdname headline
#' @export
#' @examples
#'
#' # compare_conditions() and compare_columns() produce lists that can be
#' # passed to headline_list()
#' flights_jfk %>%
#'   compare_conditions(
#'     compare = (hour > 12),
#'     reference = (hour <= 12),
#'     dep_delay
#'   ) %>%
#'   headline_list()
#'
#' # if you have more than 2 list items, you can specify them by name
#' list(
#'   x = 1,
#'   y = 2,
#'   z = 3
#'  ) %>%
#'   headline_list(
#'     compare = x,
#'     reference = z
#'   )
headline_list <- function(x,
                          headline = "{trend} of {delta} ({orig_values})",
                          compare,
                          reference,
                          ...,
                          if_match = "There was no difference.",
                          trend_phrases = headliner::trend_terms(),
                          plural_phrases = NULL,
                          orig_values = "{c} vs. {r}",
                          n_decimal = 1,
                          round_all = TRUE,
                          multiplier = 1,
                          return_data = FALSE) {
  if (missing(compare) & missing(reference)) {
    if (length(x) > 2) {
      stop(paste(
        "Not sure which columns to use, please pass list of two",
        "elements long or specify using 'compare' and 'reference'"
      ), call. = FALSE)
    }
    comp <- x[[1]][1]
    ref <- x[[2]][1]
  } else {
    comp <- x[[deparse(match.call()[["compare"]])]]
    ref <- x[[deparse(match.call()[["reference"]])]]
  }

  headline(
    compare = comp,
    reference = ref,
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

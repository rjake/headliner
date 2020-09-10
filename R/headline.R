#' @export
headline <- function(...) {
  UseMethod("headline")
}

#' Compare two values and get talking points
#'
#' @param compare numeric value to compare against reference (base) value
#' @param reference numeric value that 'compare' value will be compared against
#' @param calc string should comparison be made as the difference between the
#' two ('value', y - x) or the percent difference ('prop', (y - x) / x)
#' @param trend_phrasing list of values to use for when y is more than x, y is the
#' same as x, or y is less than x.
#' @param expr a string using \code{\link[glue]{glue}} syntax. `{c}` =
#' the 'compare' value, and `{r}` = 'reference'
#' @param n_decimal numeric value to limit the number of decimal places in
#' the returned values.
#' @param round_all logical value to indicate if all values should be rounded.
#' When FALSE, the values will return with no modification. When TRUE (default)
#' all values will be round to the length specified by 'n_decimal'.
#' @param scale number indicating the scaling factor. When scale = 1, 1/4 will
#' return 0.25, when scale = 100 (default) 1/4 will return 25
#' @importFrom glue glue
#' @importFrom purrr map_if
#' @export
#' @rdname headline
#' @seealso [view_list()] and [trend_terms()]
#' @examples
#' # manually entered
#'
#' headline(10, 8)
#' headline(10, 8, headline = "There was a ${delta} {trend} vs last year")
#' headline(
#'   compare = 10,
#'   reference = 8,
#'   headline = "Group A was {trend} by {delta_p}%.",
#'   trend_phrasing = trend_terms(more = "higher", less = "lower")
#'  )
#'
#' # a phrase about the comparion can be edited by providing glue syntax
#' # 'c' = the 'compare' value, 'r' = 'reference'
#' headline(10, 8, orig_values = "{c} to {r} people")
#'
#' # you can also adjust the rounding, although the default is 1
#' headline(0.1234, 0.4321)
#' headline(0.1234, 0.4321, n_decimal = 3)
headline.default <- function(compare,
                             reference,
                             headline = "{trend} of {delta} ({orig_values})",
                             ...,
                             trend_phrasing = headliner::trend_terms(),
                             orig_values = "{c} vs. {r}",
                             n_decimal = 1,
                             round_all = TRUE,
                             scale = 100,
                             return_data = FALSE) {
  res <-
    compare_values(
      compare = compare,
      reference = reference,
      trend_phrasing = trend_phrasing,
      orig_values = orig_values,
      n_decimal = n_decimal,
      round_all = round_all,
      scale = scale
    )

  if (return_data) {
    return(res)
  }

  glue_data(res, headline, ...)
}



#' @param x a named list with values to compare
#' @inheritParams headline.default
#' @inheritDotParams compare_values
#' @export
#' @describeIn headline Build phrase components from named list
#' @examples
#'
#' # Piping from a list (ex. compare_value())
#'
#' # First a simplified example
#' list(a = 1, b = 2) %>%
#'   headline(a, b)
#'
#' # How it is used with compare_conditions()
#' res <-
#'   flights_jfk %>%
#'   compare_conditions(
#'     compare = carrier == "AA",
#'     reference = carrier == "DL",
#'     arr_delay
#'   )
#'
#' res
#'
#' res %>%
#'   headline(
#'     mean_arr_delay_comp,
#'     mean_arr_delay_ref
#'   )
headline.list <- function(x, compare, reference, ...) {
  comp <- x[[deparse(match.call()[["compare"]])]]
  ref <- x[[deparse(match.call()[["reference"]])]]

  headline(comp, ref, ...)
}

#' @export
#' @importFrom glue glue
headline.data.frame <- function(df, compare, reference, ...) {
  if (nrow(df) > 1) {
    stop(
      glue("Data frame must be a single row. Consider using \\
      compare_conditions() or compare_columns() before using headline()"),
      call. = FALSE
    )
  }

  comp <- pull(df, {{compare}})
  ref <- pull(df, {{reference}})

  headline(comp, ref, ...)
}


# pull_vector <- function(x, col) {
#   x[[deparse(match.call()[["col"]])]]
# }
#
# pull_vector(head(mtcars), hp)
# pull_vector(list(a = 123, b = 234), a)


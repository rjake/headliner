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
#' headline(10, 8) %>% head(2)
#' headline(10, 8, calc = "prop") %>% head(2)
#' headline(10, 8, trend_phrasing = trend_terms(more = "higher")) %>%
#'   head(2)
#'
#' # a phrase about the comparion can be edited by providing glue syntax
#' # 'c' = the 'compare' value, 'r' = 'reference'
#' headline(10, 8, expr = "{c} to {r} people")$expr
#'
#' # you can also adjust the rounding, although the default is 1
#' headline(22/7, 22/3)$expr
#' headline(22/7, 22/3, n_decimal = 3)$expr
headline.default <- function(compare,
                             reference,
                             headline = "",
                             ...,
                             calc = c("value", "prop"),
                             trend_phrasing = headliner::trend_terms(),
                             expr = "{c} vs. {r}",
                             n_decimal = 1,
                             round_all = TRUE,
                             scale = 100) {
  res <-
    compare_values(
      compare = compare,
      reference = reference,
      calc = calc,
      trend_phrasing = trend_phrasing,
      expr = expr,
      n_decimal = n_decimal,
      round_all = round_all,
      scale = scale
    )

  glue_data(res, headline, ...)
}



#' @param x a named list with values to compare
#' @inheritParams headline.default
#' @inheritDotParams compare_values
#' @export
#' @describeIn headline Build phrase components from named list
#' @examples
#'
#' # Piping into compare_value() from a list
#'
#' # First a simplified example
#' list(a = 1, b = 2) %>%
#'   headline(a, b) %>%
#'   head(2)
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
#'     comp_arr_delay_mean,
#'     ref_arr_delay_mean
#'   ) %>%
#'   head(2)
headline.list <- function(x, compare, reference, ...) {
  comp <- x[[deparse(match.call()[["compare"]])]]
  ref <- x[[deparse(match.call()[["reference"]])]]

  headline(comp, ref, ...)
}

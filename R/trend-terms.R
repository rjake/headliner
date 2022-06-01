#' Phrases for direction of difference
#'
#' @description `trend_terms()` returns a list object describing the values to
#' display when `x` is greater than `y`, less than `y` or the same as `y`
#'
#' @return returns a list object
#'
#' @details `trend_terms()` will primarily be used in `headline()` and passed
#' along to `compare_conditions()`. Similar to `plural_phrasing()` Trend terms
#' can be passed in a list. See examples below.
#'
#' @param more string to use when x > y
#' @param less string to use when x < y
#' @param same string to use when x == y
#'
#' @export
#' @seealso [compare_values()]
#' @examples
#' trend_terms(same = "no change")
#'
#' headline(
#'   x = c(9, 11),
#'   y = 10,
#'   headline = "{trend} by {delta_p}%",
#'   trend_phrases = trend_terms("higher", "lower")
#' )
#'
#' # a complex example passing multiple trends and plural phrases
#' headline(
#'   35, 30,
#'   headline =
#'     "We had {an_increase} of {delta} {people}.
#'     That is {delta} {more} {employees} \\
#'     than the same time last year ({orig_values}).",
#'   trend_phrases = list(
#'     an_increase = trend_terms("an increase", "a decrease"),
#'     more = trend_terms("more", "less")
#'   ),
#'   plural_phrases =
#'     list(
#'       people = plural_phrasing("person", "people"),
#'       employees = plural_phrasing("employee", "employees")
#'     )
#' )
trend_terms <- function(more = "increase",
                        less = "decrease",
                        same = "no difference") {
  list(
    more = more,
    less = less,
    same = same
  )
}

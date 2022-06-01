#' List of values to use when change is plural (or singular)

#' @description `plural_phrasing()` returns a list object describing the value
#' to use when display when `x - y` is 1 (single) or not one (multiple or
#' fraction). This helps write "1 person" vs "2 people"
#'
#' @return returns a list object
#'
#' @details  `plural_phrasing()` will primarily be used in `headline()` and
#' passed along to `compare_conditions()`. Similar to `trend_terms()`.
#' Plural phrases can be passed in a list. See examples below.
#'
#' @param single string to use when delta = 1
#' @param multi string to use when delta > 1
#'
#' @export
#'
#' @examples
#' plural_phrasing(single = "person", multi = "people")
#'
#' headline(
#'   x = 1:2,
#'   y = 0,
#'   headline = "a difference of {delta} {people}",
#'   plural_phrases = list(people = plural_phrasing("person", "people"))
#' )
#'
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
plural_phrasing <- function(single, multi) {
  list(
    single = single,
    multi = multi
  )
}

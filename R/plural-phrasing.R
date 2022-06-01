#' List of values to use when change is plural (or singular)
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
plural_phrasing <- function(single, multi) {
  list(
    single = single,
    multi = multi
  )
}

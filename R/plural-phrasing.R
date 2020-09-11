#' List of values to use when change is plural (or singlular)
#'
#' @param single string to use when delta = 1
#' @param multi string to use when delta > 1
#'
#' @export
#'
#' @examples
#' plural_phrasing("person", "people")
plural_phrasing <- function(single, multi) {
  list(
    single = single,
    multi = multi
  )
}

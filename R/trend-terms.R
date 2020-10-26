#' Phrases for direction of difference
#'
#' @param more string to use when x > y
#' @param less string to use when x < y
#' @param same string to use when x == y
#'
#' @export
#' @seealso [compare_values()]
#' @examples
#' trend_terms(same = "no change")
trend_terms <- function(more = "increase",
                        less = "decrease",
                        same = "difference") {
  list(
    more = more,
    less = less,
    same = same
  )
}

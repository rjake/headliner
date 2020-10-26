#' Compact view of list values
#'
#' @param x list from 'compare_values()'
#' @export
#' @seealso [compare_values()]
#' @examples
#' compare_values(c(10, 8)) %>%
#'   view_list()
view_list <- function(x) {
  data.frame(
    VALUES = unlist(x)
  )
}

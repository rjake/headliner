#' Compact view of list values
#' @return returns a data frame to display a list or vector vertically
#' @param x a vector or list to be transposed
#' @export
#' @seealso [compare_values()]
#' @examples
#' compare_values(10, 8) |>
#'   view_list()
#'
#' add_article(c(1,8,10, 11, 18)) |>
#'   view_list()
view_list <- function(x) {
  data.frame(
    value = unlist(x)
  )
}

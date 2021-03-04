#' Append a/an to word
#'
#' @param x string or numeric value
#'
#' @export
#'
#' @examples
#' add_article("increase")
#' add_article("decrease")
add_article <- function(x) {
  paste(get_article(x), x)
}

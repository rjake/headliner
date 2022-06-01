#' Append a/an to word
#'
#' @return Returns a vector the same length as the input.
#'
#' @param x string or numeric value
#'
#' @export
#' @md
#' @details
#' This function uses crude logic to append 'a' or 'an' to numbers and phrases.
#' * words that start with aeiou
#' * negative numbers always start with 'a', ex: 'a -3' or 'a -8'
#' * decimals always start with 'a' ex: 0.4 is usually pronounced
#' 'a zero point four' or 'a point four'
#' * numbers starting with 8 are always 'an'
#' * if the integer that comes after thousand or million is 11 or 18 then 'an'
#'   * 18,000 becomes 18 and that becomes 'an 18'
#' * if the integer that comes after thousand or million is in
#' 1, 10, 12, 13, 14, 15, 16, 17, 19 then 'a'
#'   * 15,500 becomes 15 and that becomes 'a 15'
#' * otherwise 'a'
#'
#' @examples
#' add_article("increase")
#'
#' add_article("decrease")
#'
#' add_article(c(1, 8, 10, 11, 18, 20, 80))
#'
#' add_article(18123)
#'
#' stats::setNames(
#'   add_article(1.8 * 10^(1:7)),
#'   prettyNum(1.8 * 10^(1:7), big.mark = ",")
#' )
#'
add_article <- function(x) {
  paste(get_article(x), x)
}

#' Append a/an to word
#'
#' @param x string or numeric value
#'
#' @export
#'
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
#' add_article(c(8, 11, 18))
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


#' Choose "a" or "an"
#' Definition listed under [add_article()]
#' @param x a number or string
#' @noRd
#' @importFrom dplyr case_when
#' @examples
#' get_article("increase")
#' get_article("decrease")
#' get_article(5)
#' get_article(8)
#' get_article(18123)
#' stats::setNames(
#'   get_article(1.8 * 10^(1:7)),
#'   prettyNum(1.8 * 10^(1:7), big.mark = ",")
#' )
#'
get_article <- function(x) {

  a_patterns <- headliner_global$articles$addl_a
  an_patterns <- headliner_global$articles$addl_an

  regex_a <- glue("^({a_patterns})")
  regex_an <- glue("^({an_patterns})")

  if (is.character(x)) {
    case_when(
      nchar(a_patterns) != 0 & grepl(regex_a, x) ~ "a",
      nchar(an_patterns) != 0 & grepl(regex_an, x) ~ "an",
      grepl("^[aeiou]", tolower(x)) ~ "an",
      TRUE ~ "a"
    )
  } else {
    x_new <- case_when(
      x >= 1e6 ~ x / 1e6,
      x >= 1e3 ~ x / 1e3,
      x >= 1e2 ~ x / 1e2,
      TRUE ~ x
    ) |>
      floor()

    x_char <- as.character(x_new)
    n_char <- nchar(x_char)

    case_when(
      # -8, -6, -0.1, 0.123 = a
      grepl("^[-0]", x_char) ~ "a",
      # 100, 123, 123000 = a
      grepl("^1..$", x_char) ~ "a",
      # 80 = an
      grepl("^8", x_char)  ~ "an",
      # # 11, 18, 11000, 18000 = an
      (grepl("^1[18]", x_char) & n_char %% 2 == 0) ~ "an",
      # 1, 10, 12, 13, ... = a
      (grepl("^1", x_char) & nchar(x_char) < 3) ~ "a",
      # else
      TRUE ~ "a"
    )
  }
}

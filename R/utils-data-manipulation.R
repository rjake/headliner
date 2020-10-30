#' Find and warn for overlapping column names
#'
#' @param orig_df data frame
#' @param new_df data frame
#' @param drop when TRUE, columns with same name are dropped from the original
#' and replaced with columns from the function output.
#' @noRd
#' @examples
#' check_overlapping_names(mtcars, mtcars[,1:3])
check_overlapping_names <- function(orig_df, new_df, drop = FALSE) {
  orig_names <- names(orig_df)
  new_names <- names(new_df)
  overlap <- intersect(new_names, orig_names)

  if (length(overlap) > 0 & !drop) {
    warning(
      paste(
        "This step produced duplicate names and",
        "these fields will have new names\n",
        paste0(paste0(overlap, 1), collapse = ", "),
        "\nUse 'drop = TRUE' to remove the original columns"
      ),
      call. = FALSE
    )
  }

  overlap
}


#' Rollup data using summarise(across(...))
#'
#' @param df data frame
#' @param name prefix for
#' @param calc list of functions to be applied to variables
#' @param cond when given, data will be filtered prior to aggregation
#' @importFrom dplyr filter summarise across ungroup
#' @noRd
#' @examples
#' aggregate_group(mtcars, "", mpg, calc = list(mean = mean))
aggregate_group <- function(df, name, ..., calc, cond) {
  #df <- mtcars; cond <- dplyr::quo(cyl > 4); var <- dplyr::quo(mean(mpg))

  if (!missing(cond)) {
    df <- filter(df, {{cond}})
  }

  df %>%
    summarise(across(..., calc, .names = "{.fn}_{.col}{name}")) %>%
    ungroup() %>%
    as.list()
}


#' Choose "a" or "an"
#' @param x a number or string
#' @noRd
#' @importFrom dplyr case_when
#' @examples
#' get_article("increase")
#' get_article("decrease")
#' get_article(5)
#' get_article(8)
get_article <- function(x) {
  if (is.character(x)) {
    ifelse(grepl("^[aeiou]", tolower(x)), "an", "a")
  } else {
    x_new <- case_when(
      x >= 1e6 ~ x / 1e6,
      x >= 1e3 ~ x / 1e3,
      TRUE ~ x
    )
    x_int <- as.integer(x_new)
    x_char <- as.character(x_int)
    n_char <- nchar(x_char)

    case_when(
      # 80 = an
      grepl("^8", x_char) ~ "an",
      # # 11, 11234 = an
      (grepl("^11", x_char) & n_char %% 2 == 0) ~ "an",
      # 1, 10, 100 = a
      (grepl("^1", x_char) & nchar(x_char) <= 3) ~ "a",
      # else
      TRUE ~ "a"
    )
  }
}



#' Checks to see if rounding is causing the zero
#' @param x compare value from compare_values()
#' @param y reference value from compare_values()
#' @param n_decimal n_decimal value from compare_values()
#' @importFrom glue glue
#' @noRd
#' @examples
#' check_rounding(x = 0.2, y = 0.24, n_decimal = 1)
#' check_rounding(x = 0.2, y = 0.24, n_decimal = 2)
check_rounding <- function(x, y, n_decimal) {
  # only need if delta comes back as zero
  if (round(x - y, n_decimal) != 0) return()
  # both values need to be less than 2...  if (abs(x) + abs(y) > 2) return()

  n_decimals <- function(num) {
    n <-
      num %>%
      as.numeric() %>%  # turn 1.200 into 1.2
      as.character() %>%
      stringr::str_extract("(?<=\\.).*[^0]$") %>%
      nchar()
    ifelse(is.na(n), 0, n)
  }

  max_decimals <- max(n_decimals(c(x, y)), na.rm = TRUE)[1]

  if (n_decimal <= max_decimals & max_decimals != 0) {
    warning(
      glue(
        "With the rounding applied ('n_decimal = {n_decimal}'), \\
        your result shows a change of zero

        Your inputs had a maximum decimal length of {max_decimals} ({x}, {y}).
        Consider increasing the 'n_decimal' parameter to \\
        {max_decimals + 1} or more"
      ),
      call. = FALSE
    )
  }
}

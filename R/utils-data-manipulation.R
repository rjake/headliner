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

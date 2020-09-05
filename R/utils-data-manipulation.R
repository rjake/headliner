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
    summarise(across(..., calc, .names = "{name}{.col}_{.fn}")) %>%
    ungroup() %>%
    as.list()
}


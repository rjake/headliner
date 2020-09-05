#' Title
#'
#' @param orig_df
#' @param new_df
#' @param drop
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


#' Title
#'
#' @param df
#' @param group
#' @param fun
#' @param var
#' @importFrom dplyr filter summarise across ungroup
#'
#' @examples
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


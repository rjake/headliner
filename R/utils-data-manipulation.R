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
        "some fields have new names (see above)",
        "\nUse 'drop = TRUE' to remove the overlapping columns",
        "prior to adding the new date fields"
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
#' aggregate_group(mtcars, name = "", cols = mpg, calc = list(mean = mean))
aggregate_group <- function(df, name, cols, calc, cond) {
  #df <- mtcars; cond <- dplyr::quo(cyl > 4); var <- dplyr::quo(mean(mpg))

  if (!missing(cond)) {
    df <- filter(df, {{cond}})
  }

  df |>
    summarise(across({{cols}}, calc, .names = "{.fn}_{.col}{name}")) |>
    ungroup() |>
    as.list()
}


#' Checks to see if rounding is causing the zero
#' @param x compare value from compare_values()
#' @param y reference value from compare_values()
#' @param n_decimal n_decimal value from compare_values()
#' @importFrom utils head
#' @importFrom glue glue glue_collapse
#' @noRd
#' @examples
#' check_rounding(x = 18:30/100, y = 0.24, n_decimal = 1)
#' check_rounding(x = 0.2, y = 0.24, n_decimal = 2)
check_rounding <- function(x, y, n_decimal) {
  rounding_match <-
    which(
      x != y &
        round(x, n_decimal) == round(y, n_decimal)
    ) |>
    head(3) # only want to show a few examples in message

  n_match <- length(rounding_match)

  # stop if no matches
  if (!n_match) {
    return()
  }

  addl_info <-
    if (n_match == 1 & length(c(x, y)) == 2) {
      # only one pair submitted
      ""
    } else if (n_match == 1) {
      # only one pair matches
      paste0("(see input #", rounding_match, ")")
    } else {
      # demo list of examples
      paste0(
        "(ex: input #",
        glue_collapse(rounding_match, ", ", last = " and "),
        ")"
      )
    }

  # else
  message(
    glue(
      "With the rounding applied ('n_decimal = {n_decimal}'), \\
        result may show no change {addl_info}
        Consider increasing the 'n_decimal' parameter"
    )
  )
}

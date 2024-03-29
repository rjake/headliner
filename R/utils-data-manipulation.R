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
#' @param name prefix for
#' @param cond the x or y condition used in compare_conditions()
#' @inheritParams dplyr across
#' @importFrom rlang enquo quo_is_missing
#' @importFrom dplyr filter summarise across
#' @noRd
#' @examples
#' aggregate_group(mtcars, name = "_x", .cols = mpg, .fns = lst(mean, sd))
aggregate_group <- function(df, name, .cols, .fns, cond) {
  # df <- mtcars; name = "_x";  .cols <- as.symbol("mpg"); .fns = mean
  # cond <- dplyr::quo(cyl > 4);

  cond <- enquo(cond)

  if (!quo_is_missing(cond)) {
    df <- filter(df, {{cond}})
  }

  df |>
    summarise(
      across({{.cols}}, .fns, .names = "{.fn}_{.col}{name}")
    )
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
  if (is.character(x)) {
    ifelse(grepl("^[aeiou]", tolower(x)), "an", "a")
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

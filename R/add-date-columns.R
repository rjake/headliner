#' Add columns with date calculations based on refernce date
#'
#' @param df data frame
#' @param date_col column with class of 'date'
#' @param ref_date reference date for calculations, defaults to current date
#' @param drop some of the generated fields may match the input data frame. When
#' TRUE, the original columns will be removed and replaced with the new field
#' of the same name. Otherwise, columns with the same name will be appended with
#' a '1'
#' @importFrom dplyr pull select bind_cols
#' @importFrom tibble tibble
#' @export
#'
#' @examples
add_date_columns <- function(df,
                             date_col,
                             ref_date = Sys.Date(),
                             drop = FALSE) {
  x <- pull(df, {{date_col}})

  new_fields <-
    tibble(
      day = calc_distance(x, "day", to = ref_date),
      week = calc_distance(x, "week", to = ref_date),
      month = calc_distance(x, "month", to = ref_date),
      quarter = calc_distance(x, "month", n = 3, to = ref_date),
      calendar_year = calc_distance(x, "year", to = ref_date),
      fiscal_year =
        calc_distance(
          from = fiscal_date(x),
          unit = "year",
          to = fiscal_date(ref_date)
        )
    )

  overlap <- check_overlapping_names(df, new_fields, drop = drop)

  if (drop) {
    df <- select(df, -overlap)
  }

  bind_cols(df, new_fields)
}

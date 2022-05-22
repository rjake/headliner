#' Add columns with date calculations based on reference date
#'
#' @param df data frame
#' @param date_col column with class of 'date'
#' @param ref_date reference date for calculations, defaults to current date
#' @param fiscal_year_offset the number of months to offset date, if fiscal
#' year ends in June, use 6
#' @param week_start integer for start of week where Monday = 1 and Sunday = 7
#' @param drop some of the generated fields may match the input data frame. When
#' TRUE, the original columns will be removed and replaced with the new field
#' of the same name. Otherwise, columns with the same name will be appended with
#' a '1'
#' @importFrom dplyr pull select bind_cols
#' @importFrom tibble tibble
#' @importFrom lubridate floor_date
#' @export
#'
#' @examples
#' demo_data() %>%
#'   add_date_columns(date_col = date)
add_date_columns <- function(df,
                             date_col,
                             ref_date = Sys.Date(),
                             fiscal_year_offset = 6,
                             week_start = 1,
                             drop = FALSE) {
  x <- pull(df, {{date_col}})
  offset <- fiscal_year_offset

  new_fields <-
    tibble(
      day = calc_distance(x, "day", to = ref_date),
      week = calc_distance(x, "week", to = ref_date, week_start = week_start),
      month = calc_distance(x, "month", to = ref_date),
      quarter =
        calc_distance(
          from = floor_date(x, "quarter"),
          unit = "month",
          n = 3,
          to = floor_date(ref_date, "quarter")
        ),
      calendar_year = calc_distance(x, "year", to = ref_date),
      fiscal_year =
        calc_distance(
          from = fiscal_date(x, offset),
          unit = "year",
          to = fiscal_date(ref_date, offset)
        )
    )

  overlap <- check_overlapping_names(df, new_fields, drop = drop)

  if (drop) {
    df <- select(df, -overlap)
  }

  bind_cols(df, new_fields)
}

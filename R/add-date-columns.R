#' Title
#'
#' @param df
#' @param date_col
#' @param ref_date
#' @param drop
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

#' Title
#'
#' @param from
#' @param unit
#' @param to
#' @param n
#' @noRd
#' @importFrom lubridate floor_date interval period
#'
#' @examples
calc_distance <- function(from, unit, to = Sys.Date(), n = 1, week_start = 1) {
  from_date <- floor_date(from, unit, week_start = week_start)
  to_date <- floor_date(to, unit, week_start = week_start)
  interval(to_date, from_date) %/% period(n, unit)
}


#' Add 6 months to the date for fiscal year calculations
#'
#' @param x date
#' @param offset number of months to offset date
#' @importFrom lubridate `%m+%` period
#' @noRd
#' @examples
#' fiscal_date(as.Date("2020-01-01"))
fiscal_date <- function(x, offset) x %m+% period(offset, "months")


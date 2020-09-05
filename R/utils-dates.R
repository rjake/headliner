#' Calculate the difference between two dates
#'
#' @param from date or date-time vector to be compared against reference date
#' @param unit a character string specifying a unit of time. Valid base units
#' are second, minute, hour, day, week, month, bimonth, quarter, season,
#' halfyear and year.
#' @param to referenme date or date-time (static) for comparison
#' @param n multiple to use in \code{period(n, unit)}
#' @noRd
#' @importFrom lubridate floor_date interval period
#'
#' @examples
#' calc_distance(Sys.Date() + (-3:3), "days", Sys.Date())
#' calc_distance(Sys.time() + (-3:3) * (86400), "hours", Sys.time())
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


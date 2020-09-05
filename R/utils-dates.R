#' Title
#'
#' @param from
#' @param unit
#' @param to
#' @param n
#'
#' @importFrom lubridate floor_date interval period

#'
#' @examples
calc_distance <- function(from, unit, to = Sys.Date(), n = 1) {
  from_date <- floor_date(from, unit)
  to_date <- floor_date(to, unit)
  interval(to_date, from_date) %/% period(n, unit)
}

#' Title
#'
#' @param x
#' @importFrom lubridate `%m+%` period
#' @examples
fiscal_date <- function(x) x %m+% period(6, "months")


#' Flights from JFK Airport
#'
#' A modified subset of  \code{\link[nycflights13]{flights}}.
#'
#' @format A tibble with 12,367 rows and 10 columns:
#' \describe{
#'   \item{year}{year of arrival, 2012-2013}
#'   \item{date}{scheduled date of arrival}
#'   \item{hour}{hour of scheduled arrival, 9 AM to 9 PM}
#'   \item{carrier}{airline carrier, American Airlines (AA), Jet Blue (B6),
#'   and Delta (DL)}
#'   \item{dest}{destination, Las Vegas (LAS), Los Angeles (LAX),
#'    and San Francisco (SFO)}
#'   \item{tailnum}{tail number of plane}
#'   \item{distance}{distance from origin to destination}
#'   \item{air_time}{time in air from origin to destination}
#'   \item{dep_delay}{departure delay (minutes)}
#'   \item{arr_delay}{arrival delay (minutes)}
#'   \item{temp}{temperature (F)}
#'   \item{dewp}{dewpoint (F)}
#'   \item{humid}{relative humidity}
#'   \item{wind_dir}{wind direction (degrees)}
#'   \item{wind_speed}{wind speed (mph)}
#'   \item{precip}{precipitaton (inches)}
#'   \item{pressure}{visibility (mines)}
#'   \item{visit}{}
#' }
"flights_jfk"

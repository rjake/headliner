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
#'   \item{pressure}{sea level pressure (millibars)}
#'   \item{visib}{visibility (miles)}
#' }
"flights_jfk"



#' Animal Sleep
#'
#' A modified subset of  \code{\link[ggplot2]{msleep}}. The data originally comes from V. M. Savage and G. B. West. A quantitative, theoretical framework for understanding mammalian sleep. Proceedings of the National Academy of Sciences, 104 (3):1051-1056, 2007.
#'
#' @format A tibble with 56 rows and 7 columns:
#'    \item{name}{common name}
#'    \item{order}{Taxonomic order (between 'Class' and 'Family')}
#'    \item{hours_asleep}{total hours asleep in a 24-hour period}
#'    \item{hours_awake}{total hours awake in a 24-hour period}
#'    \item{brain_kg}{brain weight in kilograms}
#'    \item{body_kg}{body weight in kilograms}
"animal_sleep"

#' This data comes from \href{https://github.com/erictleung/pixarfilms/}{\code{pixarfilms}}
#' package by Eric Leung (2022)
#'
#' The data has box office sales, audience ratings, and release dates for each Pixar film
#'
#' @format A tibble with 22 rows and 10 columns:
#' \describe{
#'   \item{order}{order of release}
#'   \item{film}{name of film}
#'   \item{release_date}{date film premiered}
#'   \item{year}{the year the film premiered}
#'   \item{run_time}{film length in minutes}
#'   \item{film_rating}{rating based on Motion Picture Association (MPA) film
#'   rating system}
#'   \item{rotten_tomatoes}{score from the American review-aggregation website
#'     Rotten Tomatoes; scored out of 100}
#'   \item{metacritic}{score from Metacritic where scores are weighted average
#'     of reviews; scored out of 100}
#'   \item{bo_domestic}{box office gross amount in U.S. dollars (millions) for
#'     U.S. and Canada}
#'   \item{bo_intl}{box office gross amount in U.S. dollars (millions) for other
#'     territories}
#' }
#' @examples
#' pixar_films
#'
#' library(ggplot2)
#'
#' headline(
#'   x = min(pixar_films$run_time),
#'   y = max(pixar_films$run_time),
#'   headline =
#'     "The shortest film was {delta} minutes less than the longest film ({orig_values} minutes)"
#' )
#'
#' ggplot(pixar_films, aes(bo_intl, rating)) +
#'   geom_boxplot() +
#'   xlim(0, NA) +
#'   labs(title = "International Box Office by MPA Rating")
#'
#'
#' ggplot(pixar_films, aes(release_date, run_time)) +
#'   geom_line() +
#'   geom_point() +
#'   ylim(0, NA) +
#'   labs(title = "Film runtimes by release date")
#'
#'
#' ggplot(pixar_films, aes(y = reorder(film, rotten_tomatoes))) +
#'   geom_linerange(aes(xmin = rotten_tomatoes, xmax = metacritic), size = 2, color = "grey85") +
#'   geom_point(aes(x = rotten_tomatoes, color = "rotten_tomatoes")) +
#'   geom_point(aes(x = metacritic, color = "metacritic")) +
#'   scale_color_manual(values = c("steelblue1", "coral2")) +
#'   theme_minimal(base_size = 9) +
#'   labs(
#'     title = "Rotten Tomatoes vs Metacritic by film",
#'     color = NULL,
#'     y = NULL,
#'     x = "Audience Score"
#'   )
#'
"pixar_films"

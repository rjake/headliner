#' Small dataset referencing the current date
#' @param n number of rows to return
#' @param by string indicating the unit of time between dates in
#' \code{seq.Date(..., by = )}
#' @importFrom tibble tibble
#' @export
#' @examples
#' demo_data()
demo_data <- function(n = 10, by = "-60 days") {
  tibble(
    group = letters[2:(n + 1) %/% 2],
    x = 1:n + 100,
    y = 1:n * 10,
    z = 1:n %% 2,
    date = seq.Date(Sys.Date(), length.out = n, by = by)
  )
}

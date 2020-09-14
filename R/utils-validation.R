#' Check that x is vector of length 2
#' @param x vector
#' @noRd
#' @examples
#' check_valid_vector(letters)
#' check_valid_vector(1:2)
check_valid_vector <- function(x) {
  msg <- NULL

  if (!is.numeric(x)) {
    msg <- "\nexpecting a numeric vector"
  }

  if (length(x) != 2) {
    msg <- paste(msg, "expecting a length of 2", sep = "\n")
  }

  if (!is.null(msg)) {
    stop(msg, call. = FALSE)
  }
}

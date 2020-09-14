check_valid_vector <- function(x) {
  msg <- NULL

  if (!"numeric" %in% is(x)) {
    msg <- "\nexpecting a numeric vector"
  }

  if (length(x) != 2) {
    msg <- paste(msg, "expecting a length of 2", sep = "\n")
  }

  if (!is.null(msg)) {
    stop(msg, call. = FALSE)
  }
}

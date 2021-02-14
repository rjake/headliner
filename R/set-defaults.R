# ENVIRONMENT ----
# Environment that holds various global variables and settings for headliner,
# such as the current default headline syntax. It is not exported.
headliner_global <- new.env(parent = emptyenv())

# Initial default values
# The list of articles to be used for 'get_articles()'
headliner_global$articles <- list(addl_a = "", addl_an = "")

default_headline <- "{trend} of {delta} ({orig_values})"
default_orig_values <- "{c} vs. {r}"

headliner_global$headline <- default_headline
headliner_global$orig_values <- default_orig_values


# VIEW ----
#' List the defaults that headliner is using
#'
#' The default phrases used by glue might not be easy to remember. This
#' function will show the values headliner uses as default.
#' @export
#' @seealso [augment_article_patterns()], [set_headliner_defaults()]
#'
#' @examples
#' show_headliner_defaults()
show_headliner_defaults <- function() {
  message("Currently using:")

  list(
    headline = headliner_global$headline,
    orig_values = headliner_global$orig_values
  ) %>%
    print()

  message("Use set_headliner_defaults() to update or reset values.")
}


#' Update the values headliner uses in the active session
#' @noRd
update_default <- function(x, value) {
  old <- headliner_global[[x]]
  default <- get(paste0("default_", x))
  if (is.null(value)) {
    headliner_global[[x]] <- default
  } else if (length(x) > 1) {
    stop(
      "'", x, "' should be length of 1 not ", length(get(x)),
      call. = FALSE
    )
  } else {
    headliner_global[[x]] <- value
  }
}




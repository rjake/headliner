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


#' Set the default values used in making headlines
#' @param default string using component names from [compare_values()].
#' The default is \code{"{trend} of {delta} ({orig_values})"}
#' @export
#' @seealso [augment_article_patterns()], [show_headliner_defaults()]
#' @examples
#' # the way headliner writes out-of-the-box
#' headline(3, 4)
#'
#' # the values headliner uses under the hood
#' show_headliner_defaults()
#'
#' # you can use compare_values(2, 3) %>% view_list() to see all talking
#' # points but we can use this as a short cut to see some of them:
#' names(compare_values(2, 3)) %>% head()
#'
#' # update headline
#' set_headliner_defaults(
#'   headline = "{article_delta_p}% {trend} ({orig_values})",
#'   orig_values = "from {r} to {c}"
#' )
#'
#' # view changes
#' show_headliner_defaults()
#'
#' # and in action
#' headline(3, 4)
#'
#' # reset values
#' set_headliner_defaults(headline = NULL, orig_values = NULL)
#' headline(3, 4)
set_headliner_defaults <- function(headline = NULL,
                                   orig_values = NULL) {
  if (!missing(headline)) {
    update_default(x = "headline", value = headline)
  }

  if (!missing(orig_values)) {
    update_default(x = "orig_values", value = orig_values)
  }
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




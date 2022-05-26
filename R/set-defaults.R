# ENVIRONMENT ----
# Environment that holds various global variables and settings for headliner,
# such as the current default headline syntax. It is not exported.
headliner_global <- new.env(parent = emptyenv())

# Initial default values
# The list of articles to be used for 'add_article()'
headliner_global$articles <- list(addl_a = "", addl_an = "")

default_headline <- "{trend} of {delta} ({orig_values})"
default_orig_values <- "{x} vs. {y}"
default_if_match <- "There was no difference."
default_trend_phrases <- headliner::trend_terms()
default_plural_phrases <- NULL
default_n_decimal <- 1
default_round_all <- TRUE
default_multiplier <- 1

headliner_global$headline <- default_headline
headliner_global$orig_values <- default_orig_values
headliner_global$if_match <- default_if_match
headliner_global$trend_phrases <- default_trend_phrases
headliner_global$plural_phrases <- default_plural_phrases
headliner_global$orig_values <- default_orig_values
headliner_global$n_decimal <- default_n_decimal
headliner_global$round_all <- default_round_all
headliner_global$multiplier <- default_multiplier

# VIEW ----
#' List the defaults that headliner is using
#'
#' The default phrases used by glue might not be easy to remember. This
#' function will show the values headliner uses as default.
#' @export
#' @seealso [augment_article_patterns()], [set_headliner_defaults()]
#'
#' @examples
#' get_headliner_defaults()
get_headliner_defaults <- function() {
  message(
    "Use set_headliner_defaults() to update or reset values.\n",
    "Currently using:"
  )
  list(
    headline = headliner_global$headline,
    orig_values = headliner_global$orig_values,
    if_match = headliner_global$if_match,
    trend_phrases = headliner_global$trend_phrases,
    plural_phrases = headliner_global$plural_phrases,
    orig_values = headliner_global$orig_values,
    n_decimal = headliner_global$n_decimal,
    round_all = headliner_global$round_all,
    multiplier = headliner_global$multiplier
  )
}


#' Set the default values used in making headlines
#' @inheritParams compare_values
#' @export
#' @seealso [augment_article_patterns()], [get_headliner_defaults()]
#' @examples
#' # the way headliner writes out-of-the-box
#' headline(3, 4)
#'
#' # the values headliner uses under the hood
#' get_headliner_defaults()
#'
#' # you can use compare_values(2, 3) |> view_list() to see all talking
#' # points but we can use this as a short cut to see some of them:
#' names(compare_values(2, 3)) |> head()
#
#' # update headline
#' set_headliner_defaults(
#'   headline = "{article_delta_p}% {trend} ({orig_values})",
#'   orig_values = "from {y} to {x}"
#' )
#'
#' # view changes
#' get_headliner_defaults()
#'
#' # and in action
#' headline(3, 4)
#'
#' # reset values
#' set_headliner_defaults(headline = NULL, orig_values = NULL)
#' headline(3, 4)
set_headliner_defaults <- function(headline = NULL,
                                   orig_values = NULL,
                                   if_match = NULL,
                                   trend_phrases = NULL,
                                   plural_phrases = NULL,
                                   n_decimal = NULL,
                                   round_all = NULL,
                                   multiplier = NULL
                                   ) {
  if (!missing(headline)) update_default(x = "headline", value = headline)
  if (!missing(orig_values)) update_default(x = "orig_values", value = orig_values)
  if (!missing(if_match)) update_default(x = "if_match", value = if_match)
  if (!missing(trend_phrases)) update_default(x = "trend_phrases", value = trend_phrases)
  if (!missing(plural_phrases)) update_default(x = "plural_phrases", value = plural_phrases)
  if (!missing(n_decimal)) update_default(x = "n_decimal", value = n_decimal)
  if (!missing(round_all)) update_default(x = "round_all", value = round_all)
  if (!missing(multiplier)) update_default(x = "multiplier", value = multiplier)
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





# ARTICLES ----
#' Adjust which words start with "a" vs "an"
#'
#' headliner uses crude logic to anticipate the articles used before phrases
#' like "(an) increase", "(a) decrease", "(an) 83" and "(a) -5"
#'
#' These arguments assume that the words start with the patterns you provide.
#' Passing "hour|heir" will be translated to "^(hour|heir)".
#' @param regex_for_a regular expression for words that get the article 'a'
#' @param regex_for_an regular expression for words that get the article 'an'
#' @export
#' @seealso [get_headliner_defaults()], [set_headliner_defaults()]
#' @examples
#' # The crude logic for headliner would give all "h" words the article "a"
#' # by default, the word "heirloom" returns "a"
#' add_article("heirloom")
#'
#' # the patterns that are used can be updated
#' augment_article_patterns(
#'   regex_for_a = "Euro|uni",
#'   regex_for_an = "hour|heir"
#' )
#'
#' add_article("heirloom")
#'
#' # these patterns can be reset using 'NULL'
#' augment_article_patterns(
#'   regex_for_an = NULL
#' )
#'
#' add_article("heirloom")
augment_article_patterns <- function(regex_for_a, regex_for_an) {
  old <- headliner_global$articles

  if (!missing(regex_for_a)) {
    a_patterns <- validate_augment_articles(regex_for_a)
    headliner_global$articles$addl_a <- a_patterns
  }

  if (!missing(regex_for_an)) {
    an_patterns <- validate_augment_articles(regex_for_an)
    headliner_global$articles$addl_an <- an_patterns
  }
}


validate_augment_articles <- function(x = NULL) {
  if (length(x) > 1) {
    stop(
      "patterns should be a single regular expression",
      call. = FALSE
    )
  }

  if (is.null(x)) {
    return("")
  } else {
    return(x)
  }
}


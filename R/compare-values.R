#' Compare two values and get talking points
#'
#' @param compare numeric value to compare against reference (base) value
#' @param reference numeric value that 'compare' value will be compared against
#' @param calc string should comparison be made as the difference between the
#' two ('value', y - x) or the percent difference ('prop', (y - x) / x)
#' @param trend_phrasing list of values to use for when y is more than x, y is the
#' same as x, or y is less than x.
#' @param plural_phrases named list of values to use when difference (delta) is
#' singular (delta = 1) or plural (delta != 1)
#' @param orig_values a string using \code{\link[glue]{glue}} syntax. `{c}` =
#' the 'compare' value, and `{r}` = 'reference'
#' @param n_decimal numeric value to limit the number of decimal places in
#' the returned values.
#' @param round_all logical value to indicate if all values should be rounded.
#' When FALSE, the values will return with no modification. When TRUE (default)
#' all values will be round to the length specified by 'n_decimal'.
#' @param scale number indicating the scaling factor. When scale = 1, 1/4 will
#' return 0.25, when scale = 100 (default) 1/4 will return 25
#' @importFrom glue glue
#' @importFrom purrr map_if map pluck
#' @export
#' @rdname compare_values
#' @seealso [headline()], [view_list()] and [trend_terms()]
#' @examples
#' # the values can be manually entered
#'
#' compare_values(10, 8) %>% head(2)
#' # percent difference (10-8)/8
#' compare_values(10, 8)$delta_p
#' compare_values(10, 8, trend_phrasing = trend_terms(more = "higher")) %>%
#'   head(2)
#'
#' # a phrase about the comparion can be edited by providing glue syntax
#' # 'c' = the 'compare' value, 'r' = 'reference'
#' compare_values(10, 8, orig_values = "{c} to {r} people")$orig_values
#'
#' # you can also adjust the rounding, although the default is 1
#' compare_values(0.1234, 0.4321)$orig_values
#' compare_values(0.1234, 0.4321, n_decimal = 3)$orig_values
compare_values <- function(compare,
                           reference,
                           trend_phrasing = headliner::trend_terms(),
                           orig_values = "{c} vs. {r}",
                           plural_phrases = NULL,
                           n_decimal = 1,
                           round_all = TRUE,
                           scale = 100) {
  calc <- compare - reference
  calc_p <- (compare - reference) / reference  * scale

  sign_calc <- sign(calc)

  phrase <-
    switch(
      as.character(sign_calc), # must be a character
      "1" = trend_phrasing$more,
      "-1" = trend_phrasing$less,
      "0" = trend_phrasing$same
    )


  output <-
    list(
      delta = abs(calc),
      trend = phrase,
      delta_p = abs(calc_p),
      article_delta = get_article(abs(calc)),
      article_delta_p = get_article(abs(calc_p)),
      article_trend = get_article(phrase),
      comp_value = compare,
      ref_value = reference,
      raw_delta = calc,
      raw_delta_p = calc_p,
      sign = sign_calc,
      orig_values = glue(
        orig_values,
        c = round(compare, n_decimal),
        r = round(reference, n_decimal)
      )
    )

  if (round_all) {
    output <-
      output %>%
      map_if(is.numeric, round, n_decimal)
  }

  if (!is.null(plural_phrases)) {
    # stop if items in list aren't named
    if (any(is.null(names(plural_phrases)))) {
      stop(paste0(
        "'plural_phrases' should be a named list. \nex. ",
        'list(people = plural_phrasing("person", "people"))'
        ), call. = FALSE)
    }
    # identify which list element to choose if delta = 1 then single else multi
    value <- ifelse(output$delta == 1, "single", "multi")
    # append to list
    output <- append(output, map(plural_phrases, pluck, value))
  }

  output
}


#' Compact view of list values
#'
#' @param x list from 'compare_values()'
#' @export
#' @seealso [compare_values()]
#' @examples
#' compare_values(10, 8) %>%
#'   view_list()
view_list <- function(x) {
  data.frame(
    VALUES = unlist(x)
  )
}


#' Phrases for direction of difference
#'
#' @param more string to use when x > y
#' @param less string to use when x < y
#' @param same string to use when x == y
#'
#' @export
#' @seealso [compare_values()]
#' @examples
#' trend_terms(same = "no change")
trend_terms <- function(more = "increase",
                        less = "decrease",
                        same = "difference") {
  list(
    more = more,
    less = less,
    same = same
  )
}


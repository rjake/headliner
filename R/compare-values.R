#' Compare two values and get talking points
#'
#' @param compare a numeric value to compare to a reference value
#' @param reference a numeric value to act as a control for the 'compare' value
#' @param trend_phrases list of values to use for when y is more than x, y is the
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
#' @param multiplier number indicating the scaling factor. When multiplier = 1
#' (default), 0.25 will return 0.25. When multiplier = 100, 0.25 will return 25.
#' @importFrom glue glue
#' @importFrom purrr map_if map pluck
#' @importFrom dplyr recode
#' @export
#' @rdname compare_values
#' @seealso [view_list()], [trend_terms()], and [plural_phrasing()]
#' @examples
#' # the values can be manually entered
#'
#' compare_values(10, 8) %>% head(2)
#' # percent difference (10-8)/8
#' compare_values(10, 8)$delta_p
#' compare_values(10, 8, trend_phrases = trend_terms(more = "higher")) %>%
#'   head(2)
#'
#' # a phrase about the comparion can be edited by providing glue syntax
#' # 'c' = the 'compare' value, 'r' = 'reference'
#' compare_values(10, 8, orig_values = "{c} to {r} people")$orig_values
#'
#' # you can also adjust the rounding, although the default is 1
#' compare_values(0.1234, 0.4321)$orig_values
#' compare_values(0.1234, 0.4321, n_decimal = 3)$orig_values
#' # or add a multiplier
#' compare_values(0.1234, 0.4321, multiplier = 100)$orig_values
compare_values <- function(compare, reference,
                           trend_phrases = headliner::trend_terms(),
                           orig_values = "{c} vs. {r}",
                           plural_phrases = NULL,
                           n_decimal = 1,
                           round_all = TRUE,
                           multiplier = 1) {
  # calcs
  comp <- (compare * multiplier)
  ref <- (reference * multiplier)

  delta <- as.numeric(comp - ref)
  delta_p <- as.numeric(delta / ref  * 100)

  calc <-
    list(
      delta = delta,
      delta_p = delta_p,
      sign = sign(delta),
      abs_delta = abs(delta),
      abs_delta_p = abs(delta_p),
      compare = comp,
      reference = ref
    )

  if (round_all) {
    # give a warning if rounding causes a delta of 0 due to inputs having
    # decimals >= n_decimal parameter
    check_rounding(
      x = calc$compare,
      y = calc$reference,
      n_decimal = n_decimal
    )

    calc <-
      calc %>%
      map_if(is.numeric, round, n_decimal)
  }


  which_trend <-
    recode(
      as.character(calc$sign), # must be a character
      "1" = "more",
      "-1" = "less",
      "0" = "same"
    )

  trend <- trend_phrases[[which_trend]]


  output <-
    list(
      delta = calc$abs_delta,
      trend = trend,
      delta_p = calc$abs_delta_p,
      article_delta = paste(get_article(calc$abs_delta), calc$abs_delta),
      article_delta_p = paste(get_article(calc$abs_delta_p), calc$abs_delta_p),
      article_trend = paste(get_article(trend), trend),
      comp_value = calc$compare,
      ref_value = calc$reference,
      raw_delta = calc$delta,
      raw_delta_p = calc$delta_p,
      article_raw_delta = paste(get_article(calc$delta), calc$delta),
      article_raw_delta_p = paste(get_article(calc$delta_p), calc$delta_p),
      sign = calc$sign,
      orig_values = glue(
        orig_values,
        c = calc$compare,
        r = calc$ref
      )
    )

  # append plural phrases if provided
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



#' identify trend terms to use
#' @noRd
#' @importFrom purrr map pluck
#' @examples
#' return_trend_phrases(trend_terms())
#' return_trend_phrases(list(trend = trend_terms()))
#' return_trend_phrases(
#'   list(trend = trend_terms(), x = trend_terms(), y = trend_terms())
#' )
return_trend_phrases <- function(trend_phrases, sign = 1) {
  # trend_terms() returns a list, this test is:
  # FALSE when passed alone
  # TRUE when used in a list of trend terms
  passed_as_list <- is.list(trend_phrases[[1]])

  if(passed_as_list) {
    trend_list <- trend_phrases
  } else {
    # else make list
    trend_list <- list(trend = trend_phrases)
  }

  which_trend <-
    switch(
      as.character(sign),
      "1" = "more",
      "-1" = "less",
      "0" = "same"
    )

  map(trend_list, pluck, which_trend)
}

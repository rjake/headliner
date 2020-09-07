#' @export
build_phrase <- function(...) {
  UseMethod("build_phrase")
}


#' Build phrase components
#'
#' @param compare numeric value to compare against reference (base) value
#' @param reference numeric value that 'compare' value will be compared against
#' @param calc string should comparison be made as the difference between the
#' two ('value', y - x) or the percent difference ('prop', (y - x) / x)
#' @param phrasing list of values to use for when y is more than x, y is the
#' same as x, or y is less than x.
#' @param expr a string using \code{\link[glue]{glue}} syntax
#' @param n_decimal numeric value to limit the number of decimal places in
#' the returned values.
#' @param round_all logical value to indicate if all values should be rounded.
#' When FALSE, the values will return with no modification. When TRUE (default)
#' all values will be round to the length specified by 'n_decimal'.
#' @param scale number indicating the scaling factor. When scale = 1, 1/4 will
#' return 0.25, when scale = 100 (default) 1/4 will return 25
#' @importFrom glue glue
#' @importFrom purrr map_if
#' @export
#' @rdname build_phrase
#' @seealso [view_components()] and [phrase_terms()]
#' @examples
#' # manually entered
#'
#' build_phrase(10, 8) %>% head(2)
#' build_phrase(10, 8, calc = "prop") %>% head(2)
#' build_phrase(10, 8, phrasing = phrase_terms(more = "higher")) %>% head(2)
#'
#' # a phrase about the comparion can be edited by providing glue syntax
#' # '{c}' = 'compare' value, '{r}' = 'reference'
#' build_phrase(10, 8, expr = "{c} to {r} people")$expr
#'
#' # you can also adjust the rounding, although the default is 1
#' build_phrase(22/7, 22/3)$expr
#' build_phrase(22/7, 22/3, n_decimal = 3)$expr
build_phrase.default <- function(compare,
                                 reference,
                                 calc = c("value", "prop"),
                                 phrasing = headliner::phrase_terms(),
                                 expr = "{c} vs. {r}",
                                 n_decimal = 1,
                                 round_all = TRUE,
                                 scale = 100) {
  calc <- match.arg(calc)

  if (calc == "value") {
    res <- compare - reference
  } else {
    res <- (compare - reference) / reference  * scale
  }

  sign_res <- sign(res)

  phrase <-
    switch(
      as.character(sign_res), # must be a character
      "1" = phrasing$more,
      "-1" = phrasing$less,
      "0" = phrasing$same
    )


  output <-
    list(
      delta = abs(res),
      phrase = phrase,
      comp_value = compare,
      ref_value = reference,
      raw_delta = res,
      sign = sign_res,
      calc = calc,
      expr = glue(
        expr,
        c = round(compare, n_decimal),
        r = round(reference, n_decimal)
      )
    )

  if (round_all) {
    output <-
      output %>%
      map_if(is.numeric, round, n_decimal)
  }

  output
}

#' @param x a named list with values to compare
#' @export
#' @describeIn build_phrase Build phrase components from named list
#' @examples
#'
#' # Using a list as will be output of compare_conditions()
#'
#' # First a simplified example
#' list(a = 1, b = 2) %>%
#'   build_phrase(a, b) %>%
#'   head(2)
#'
#' # How it is used with compare_conditions()
#' res <-
#'   flights_jfk %>%
#'   compare_conditions(
#'     compare = carrier == "AA",
#'     reference = carrier == "DL",
#'     arr_delay
#'   )
#'
#' res
#'
#' res %>%
#'   lapply(round, 1) %>% # prevents endless decimal places
#'   build_phrase(
#'     comp_arr_delay_mean,
#'     ref_arr_delay_mean
#'   ) %>%
#'   head(2)
build_phrase.list <- function(x, compare, reference, ...) {
  comp <- x[[deparse(match.call()[["compare"]])]]
  ref <- x[[deparse(match.call()[["reference"]])]]

  build_phrase(comp, ref, ...)
}


#' Compact view of phrase components
#'
#' @param x list from 'build_phrase()'
#' @export
#' @seealso [build_phrase()]
#' @examples
#' build_phrase(10, 8) %>%
#'   view_components()
view_components <- function(x) {
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
#' @seealso [build_phrase()]
#' @examples
#' phrase_terms(same = "no change")
phrase_terms <- function(more = "increase",
                         less = "decrease",
                         same = "difference") {
  list(
    more = more,
    less = less,
    same = same
  )
}


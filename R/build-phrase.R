
#' Title
#'
#' @param x
#' @param ...
#'
#' @export
#'
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
#'
#' @export
#' @rdname build_phrase
#' @seealso [view_components()] and [phrase_terms()]
#' @examples
#' build_phrase(10, 8) %>% head(2)
#' build_phrase(10, 8, calc = "prop") %>% head(2)
#' build_phrase(10, 8, phrasing = phrase_terms(more = "higher")) %>% head(2)
build_phrase.default <- function(compare,
                                 reference,
                                 calc = c("value", "prop"),
                                 phrasing = headliner::phrase_terms()
                                 ) {
  calc <- match.arg(calc)

  if (calc == "value") {
    res <- compare - reference
  } else {
    res <- (compare - reference) / reference
  }

  sign_res <- sign(res)

  phrase <-
    switch(
      as.character(sign_res), # must be a character
      "1" = phrasing$more,
      "-1" = phrasing$less,
      "0" = phrasing$same
    )


  list(
    delta = abs(res),
    phrase = phrase,
    comp_value = compare,
    ref_value = reference,
    raw_delta = res,
    sign = sign_res,
    calc = calc,
    expr_comp = deparse(match.call()[["compare"]]),
    expr_ref = deparse(match.call()[["reference"]])
  )
}

#' @param x a named list with values to compare
#' @export
#' @describeIn build_phrase Build phrase components from named list
#' @examples
#' build_phrase(list(a = 1, b = 2), a, b) %>% head(2)
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
#' build_phrase(10, 8) %>% view_components()
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


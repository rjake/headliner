#' Compose phrases that describe differences in the data
#' @export
headline <- function(...) {
  UseMethod("headline")
}

#' @param compare a numeric value to compare to a reference value
#' @param reference a numeric value to act as a control for the 'compare' value
#' @param headline a string to format the final output. Uses
#' \code{\link[glue]{glue}} syntax
#' @param ... arguments passed to \code{\link[glue]{glue_data}}
#' @param if_match string to display if numbers match, uses
#' \code{\link[glue]{glue}} syntax
#' @param trend_phrases list of values to use for when y is more than x, y is the
#' same as x, or y is less than x.
#' @param plural_phrases named list of values to use when difference (delta) is
#' singular (delta = 1) or plural (delta != 1)
#' @param orig_values a string to display the two original values. Uses
#'  \code{\link[glue]{glue}} syntax. `{c}` = the 'compare' value, and
#'  `{r}` = 'reference'
#' @param n_decimal numeric value to limit the number of decimal places in
#' the returned values.
#' @param round_all logical value to indicate if all values should be rounded.
#' When FALSE, the values will return with no modification. When TRUE (default)
#' all values will be round to the length specified by 'n_decimal'.
#' @param multiplier number indicating the scaling factor. When multiplier = 1
#' (default), 0.25 will return 0.25. When multiplier = 100, 0.25 will return 25.
#' @param return_data logical to indicate whether function should return the
#' phrase components used to compose the headline
#' @importFrom glue glue_data
#' @importFrom purrr map_if
#' @export
#' @rdname headline
#' @seealso [view_list()] and [trend_terms()]
#' @examples
#' # values can be manually entered, some headlines are provided by default
#' headline(10, 8)
#' headline(8, 10)
#' headline(10, 10)
#'
#' # most likely you'll edit the headline by hand
#' headline(
#'   compare = 10,
#'   reference = 8,
#'   headline = "There was a ${delta} {trend} vs last year"
#' )
#'
#' # you can also adjust the phrasing of higher/lower values
#' headline(
#'   compare = 10,
#'   reference = 8,
#'   headline = "Group A was {trend} by {delta_p}%.",
#'   trend_phrases = trend_terms(more = "higher", less = "lower")
#'  )
#'
#' # a phrase about the comparion can be edited by providing glue syntax
#' # 'c' = the 'compare' value, 'r' = 'reference'
#' headline(10, 8, orig_values = "{c} to {r} people")
#'
#' # you can also add phrases for when the difference = 1 or not
#' headline(
#'   compare = 10,
#'   reference = 8,
#'   plural_phrases = list(
#'     were = plural_phrasing(single = "was", multi = "were"),
#'     people = plural_phrasing(single = "person", multi = "people")
#'   ),
#'   headline = "there {were} {delta} {people}"
#' )
#'
#' # you can also adjust the rounding, although the default is 1
#' headline(0.1234, 0.4321)
#' headline(0.1234, 0.4321, n_decimal = 3)
#' # or use a multiplier
#' headline(0.1234, 0.4321, multiplier = 100)
#'
#' # there are many components you can assemble
#' headline(
#'   compare = 16,
#'   reference = 8,
#'   headline = "there was {article_delta_p}% {trend}, \\
#'   {article_trend} {trend} of {delta} ({orig_values})"
#' )
#'
#' # compare_conditions() produces a list that can be passed to headline()
#'  mtcars %>%
#'    compare_conditions(
#'      compare = cyl == 4,
#'      reference = cyl == 6,
#'      cols = c(mpg)
#'    ) %>%
#'    headline(
#'      headline =
#'        "4-cylinder cars get an average of {delta} {trend} miles \\
#'        per gallon than 6-cylinder cars ({orig_values}).",
#'      trend_phrases = trend_terms("more", "less")
#'    )
#'
headline.default <- function(compare,
                             reference,
                             headline = "{trend} of {delta} ({orig_values})",
                             ...,
                             if_match = "There was no difference.",
                             trend_phrases = headliner::trend_terms(),
                             plural_phrases = NULL,
                             orig_values = "{c} vs. {r}",
                             n_decimal = 1,
                             round_all = TRUE,
                             multiplier = 1,
                             return_data = FALSE) {
  res <-
    compare_values(
      compare,
      reference,
      trend_phrases = trend_phrases,
      plural_phrases = plural_phrases,
      orig_values = orig_values,
      n_decimal = n_decimal,
      round_all = round_all,
      multiplier = multiplier
    )


  if (return_data) {
    res <- append(res, list(headline = glue_data(res, headline)))
    return(res)
  }

  # determine which headline phrasing to use
  final_output <- glue_data(res, headline, ...)
  final_output[res$sign == 0] <- glue_data(res, if_match, ...)

  final_output
}


#' @param x a list with values to compare, if named, can call by name
#' @param compare numeric value to compare against reference (base) value
#' @param reference numeric value that 'compare' value will be compared against
#' @inheritDotParams headline.default
#' @rdname headline
#' @export
headline.list <- function(x, compare, reference, ...) {
  if (missing(compare) & missing(reference)) {
    if (length(x) > 2) {
      stop(paste(
        "Not sure which columns to use, please pass list of two",
        "elements long or specify using 'compare' and 'reference'"
      ), call. = FALSE)
    }
    comp <- x[[1]][1]
    ref <- x[[2]][1]
  } else {
    comp <- x[[deparse(match.call()[["compare"]])]]
    ref <- x[[deparse(match.call()[["reference"]])]]
  }

  headline(comp, ref, ...)
}

#' Add column of headlines
#' @param x data frame, must be a single row
#' @param compare numeric value to compare against reference (base) value
#' @param reference numeric value that 'compare' value will be compared against
#' @param .name string value for the name of the new column to create
#' @inheritDotParams headline.default
#' @rdname headline
#' @export
#' @importFrom glue glue
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom purrr map2_chr
#' @examples
#'
#' # You can use 'add_headline_column()' instead of
#' # `mutate(headline = map2_chr(...))`
#' # here is an example comparing the # of gears and carburetors in the
#' # 'mtcars' data set
#' head(mtcars, 8) %>%
#'   dplyr::select(mpg, cyl, gear, carb) %>%
#'   add_headline_column(
#'     compare = gear,
#'     reference = carb
#'   )
add_headline_column <- function(x, compare, reference, ..., .name = "headline") {
  if (missing(compare) | missing(reference)) {
    stop(
      "please specify columns using 'compare' and 'reference'",
      call. = FALSE
    )
  }

  if (.name %in% names(x)) {
    warning(
      glue(
        "The column '{.name}' was replaced. Use the '.name' argument \\
        to change the column name."
      ),
      call. = FALSE
    )
  }

  x %>%
    mutate(
      {{.name}} := map2_chr({{compare}}, {{reference}}, headline, ...)
    )
}


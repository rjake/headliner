#' @export
headline <- function(...) {
  UseMethod("headline")
}

#' Compare two values and get talking points
#'
#' @param compare numeric value to compare against reference (base) value
#' @param reference numeric value that 'compare' value will be compared against
#' @param headline a string to format the final output. Uses
#' \code{\link[glue]{glue}} syntax
#' @param if_match string to display if numbers match, uses
#' \code{\link[glue]{glue}} syntax
#' @param trend_phrasing list of values to use for when y is more than x, y is the
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
#' @param scale number indicating the scaling factor. When scale = 1, 1/4 will
#' return 0.25, when scale = 100 (default) 1/4 will return 25
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
#'   trend_phrasing = trend_terms(more = "higher", less = "lower")
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
#'
#' # The values can come from a summarized data frame or a named list
#' # if the data frame is only 2 columns or the list has only 2 elements
#' # you don't need to specify the 'compare' or 'reference' arguments unless
#' # you need to change the order. If the # of columns is > 3 you'll need to
#' # specify these arguments.
#' iris %>%
#'   dplyr::summarise_at(dplyr::vars(Sepal.Length, Petal.Length), mean) %>%
#'   headline()
#'
#' iris %>%
#'   dplyr::summarise_at(dplyr::vars(Sepal.Length, Petal.Length), mean) %>%
#'   headline(
#'     compare = Petal.Length,
#'     reference = Sepal.Length
#'   )
#'
#' # there are many components you can assemble
#' headline(
#'   16, 8,
#'   headline = "there was {article_delta_p} {delta_p}% {trend}, \\
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
#'      trend_phrasing = trend_terms("more", "less")
#'    )
headline.default <- function(compare,
                             reference,
                             headline = "{trend} of {delta} ({orig_values})",
                             ...,
                             if_match = "There was no difference.",
                             trend_phrasing = headliner::trend_terms(),
                             plural_phrases = NULL,
                             orig_values = "{c} vs. {r}",
                             n_decimal = 1,
                             round_all = TRUE,
                             scale = 100,
                             return_data = FALSE) {
  res <-
    compare_values(
      compare = compare,
      reference = reference,
      trend_phrasing = trend_phrasing,
      plural_phrases = plural_phrases,
      orig_values = orig_values,
      n_decimal = n_decimal,
      round_all = round_all,
      scale = scale
    )

  if (res$sign == 0) {
    headline <- if_match
  }

  if (return_data) {
    res <- append(res, list(headline = glue_data(res, headline)))
    return(res)
  }

  glue_data(res, headline, ...)
}


#' For a list
#' @param x a list with values to compare, if named, can call by name
#' @inheritParams headline.default
#' @inheritDotParams compare_values
#' @describeIn headline.default for lists
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

#' For a data frame
#' @param x data frame, must be a single row
#' @describeIn headline.default for data frames
#' @export
#' @importFrom glue glue
headline.data.frame <- function(x, compare, reference, ...) {
  if (nrow(x) > 1) {
    stop(
      glue("Data frame must be a single row. Consider using \\
      compare_conditions() or compare_columns() before using headline()"),
      call. = FALSE
    )
  }

  if (missing(compare) & missing(reference)) {
    if (length(x) > 2) {
      stop(paste(
        "Not sure which columns to use, please pass data frame of two",
        "columns or specify using 'compare' and 'reference'"
      ), call. = FALSE)
    }

    comp <- x[[1]][1]
    ref <- x[[2]][1]
  } else {
    comp <- pull(x, {{compare}})
    ref <- pull(x, {{reference}})
  }

  headline(comp, ref, ...)
}


# pull_vector <- function(x, col) {
#   x[[deparse(match.call()[["col"]])]]
# }
#
# pull_vector(head(mtcars), hp)
# pull_vector(list(a = 123, b = 234), a)


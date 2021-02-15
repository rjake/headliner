#' Add column of headlines
#' @param x data frame, must be a single row
#' @param compare numeric value to compare against reference (base) value
#' @param reference numeric value that 'compare' value will be compared against
#' @param .name string value for the name of the new column to create
#' @inheritDotParams headline.default
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


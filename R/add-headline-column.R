#' Add column of headlines
#' @param df data frame, must be a single row
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
add_headline_column <- function(df,
                                compare,
                                reference,
                                headline = "{trend} of {delta} ({orig_values})",
                                ...,
                                .name = "headline",
                                if_match = "There was no difference.",
                                trend_phrases = headliner::trend_terms(),
                                plural_phrases = NULL,
                                orig_values = "{c} vs. {r}",
                                n_decimal = 1,
                                round_all = TRUE,
                                multiplier = 1,
                                return_data = FALSE) {
  if (missing(compare) | missing(reference)) {
    stop(
      "please specify columns using 'compare' and 'reference'",
      call. = FALSE
    )
  }

  if (.name %in% names(df)) {
    warning(
      glue(
        "The column '{.name}' was replaced. Use the '.name' argument \\
        to change the column name."
      ),
      call. = FALSE
    )
  }

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

  df %>%
    mutate(
      {{.name}} := map2_chr({{compare}}, {{reference}}, headline, ...)
    )
}


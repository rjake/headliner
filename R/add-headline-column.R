#' Add column of headlines
#' @param df data frame, must be a single row
#' @param compare numeric value to compare against reference (base) value
#' @param reference numeric value that 'compare' value will be compared against
#' @param .name string value for the name of the new column to create
#' @param return_cols arguments that can be passed to
#' \code{\link[dplyr]{select}}, ex: c("a", "b"),
#' \code{\link[dplyr]{starts_with}},etc.
#' @inheritParams headline.default
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
add_headline_column <- function(df,
                                compare,
                                reference,
                                headline = "{trend} of {delta} ({orig_values})",
                                #...,
                                .name = "headline",
                                if_match = "There was no difference.",
                                trend_phrases = headliner::trend_terms(),
                                plural_phrases = NULL,
                                orig_values = "{c} vs. {r}",
                                n_decimal = 1,
                                round_all = TRUE,
                                multiplier = 1,
                                return_data = FALSE) {

  # confirm args listed
  if (missing(compare) | missing(reference)) {
    abort("please specify columns using 'compare' and 'reference'")
  }

  # inform that headline can be renamed
  if (.name %in% names(df)) {
    glue(
      "The column '{.name}' was replaced. Use the '.name' argument \\
      to change the column name."
    ) %>%
    warn()
  }

  # pass values and unnest ----
  new_cols <-
    df %>%
    transmute(
      comp_values = # returns a list per row
        map2(
          .x = {{compare}},
          .y = {{reference}},
          .f =
            ~compare_values(
              .x,
              .y,
              trend_phrases = trend_phrases,
              plural_phrases = plural_phrases,
              orig_values = orig_values,
              n_decimal = n_decimal,
              round_all = round_all,
              multiplier = multiplier
            ) %>%
            as.data.frame()
          )
      ) %>%
      unnest(.data$comp_values)

  # combine with original data
  full_data <- bind_cols(df, new_cols)

  # create headline column
  headline_col <-
    full_data %>%
    transmute({{.name}} := glue(headline))

  # return data ----
  if (return_data) {
    # warn if names overlap
    overlapping_names <- names(new_cols)[names(new_cols) %in% names(df)]
    if (length(overlapping_names)) {
      paste0(
        "The following columns were replaced:\n",
        paste(paste("-", overlapping_names), collapse = "\n")
      ) %>%
        warn()
    }

    final_df <-
      df %>%
      select(-any_of(names(new_cols))) %>%
      bind_cols(headline_col, new_cols)

    return(final_df)
  }

  # otherwise just append headline column to orig data
  df %>%
    bind_cols(headline_col)
}


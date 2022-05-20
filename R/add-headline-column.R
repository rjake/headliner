#' Add column of headlines
#'
#' @param df data frame, must be a single row
#' @param .name string value for the name of the new column to create
#' @param return_cols arguments that can be passed to
#' \code{\link[dplyr]{select}}, ex: c("a", "b"),
#' \code{\link[dplyr]{starts_with}},etc.
#' @inheritParams compare_values
#' @inheritParams headline
#' @export
#' @importFrom glue glue
#' @importFrom dplyr mutate transmute bind_cols any_of select
#' @importFrom tidyr unnest
#' @importFrom rlang := .data abort warn
#' @importFrom purrr map2
#' @examples
#'
#' # You can use 'add_headline_column()' instead of
#' # `mutate(headline = map2_chr(...))`
#' # here is an example comparing the sleeping habits of animals
#' head(animal_sleep) %>%
#'   dplyr::select(common_name, hours_asleep, hours_awake) %>%
#'   add_headline_column(
#'     x = hours_asleep,
#'     y = hours_awake,
#'     headline = "The {common_name} spends {delta} more {hours} {trend} than not {trend}.",
#'     trend_phrases = trend_terms(more = "asleep", less = "awake"),
#'     plural_phrases = list(hours = plural_phrasing(single = "hour", multi = "hours"))
#'   ) %>%
#'   knitr::kable("pandoc")
#'
#'
#' # you can also use 'return_cols' to return any and all "talking points".
#' # You can use tidyselect helpers like 'starts_with("delta")' or
#' # 'everything()'. In this example, I returned the delta & trend columns
#' # and identified the rows at the extremes
#' head(animal_sleep) %>%
#'   dplyr::select(common_name, hours_asleep, hours_awake) %>%
#'   add_headline_column(
#'     x = hours_asleep,
#'     y = hours_awake,
#'     headline = "more time {trend} ({orig_values} hours)",
#'     trend_phrases = trend_terms(more = "alseep", less = "awake"),
#'     return_cols = c("delta", "trend")
#'   ) %>%
#'   dplyr::filter(delta %in% range(delta)) %>%
#'   knitr::kable("pandoc")
#'
add_headline_column <- function(df,
                                x,
                                y,
                                headline = "{trend} of {delta} ({orig_values})",
                                ...,
                                .name = "headline",
                                if_match = "There was no difference.",
                                trend_phrases = headliner::trend_terms(),
                                plural_phrases = NULL,
                                orig_values = "{x} vs. {y}",
                                n_decimal = 1,
                                round_all = TRUE,
                                multiplier = 1,
                                return_cols = .name) {
  # df <- mtcars; x = as.symbol("gear"); y = as.symbol("carb")

  # inform that headline can be renamed
  if (.name %in% names(df)) {
    glue(
      "The column '{.name}' was replaced. Use the '.name' argument \\
      to change the new column name."
    ) %>%
    warn()
  }

  headline_pattern <- headline

  df %>%
    mutate(
      comp_values = # returns a list per row
        map2(
          .x = {{x}},
          .y = {{y}},
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
            )
        ) %>%
        map_dfr(flatten) %>%
        mutate(
          {{.name}} :=
            ifelse(
              test = .data$x == .data$y,
              yes = if_match,
              no = glue(headline_pattern, ...)
            )
        ) %>%
        select({{.name}}, {{return_cols}})
      ) %>%
      unnest_wider(
        .data$comp_values,
        names_repair = "unique"
      )
}

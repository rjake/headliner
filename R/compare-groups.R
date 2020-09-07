#' Compare two conditions within a data frame
#'
#' @param df data frame
#' @param compare condition for comparison, same criteria you would use in
#' 'dplyr::filter'
#' @param reference same as compare
#' @param ... columns to use in comparison
#' @param calc named list of the functions to use, ex:
#' list(mean = mean, sd = sd) 'purrr' style phrases are also supported like
#' list(mean = ~mean(.x, na.rm = TRUE), sd = sd)
#'
#' @export
#'
#' @examples
#' flights_jfk %>%
#'   compare_conditions(
#'     compare = (carrier == "AA"),
#'     reference = complete.cases(.),
#'     c(dep_delay, arr_delay),
#'     calc = list(mean = mean, sd = sd)
#'   ) %>%
#'   view_components()
#'
#' mtcars %>%
#'   compare_conditions(
#'     compare = (hp > 100 | gear == 4),
#'     reference = (mpg > 24),
#'     dplyr::starts_with("d"),
#'     calc = list(mean = mean, max = max)
#'   ) %>%
#'   view_components()
compare_conditions <- function(df,
                               compare,
                               reference,
                               ...,
                               calc = list(mean = mean)
                               ) {
  res_1 <- aggregate_group(df, "comp_", ..., calc = calc, cond = {{compare}})
  res_2 <- aggregate_group(df, "ref_", ..., calc = calc, cond = {{reference}})
  append(res_1, res_2)
}


#' Compare two columns within a data frame
#'
#' @param df data frame
#' @param ... columns to compare, supports select helpers like 'starts_with()'
#' or 'where(is.numeric)'
#' @param calc named list of the functions to use, ex:
#' list(mean = mean, sd = sd) 'purrr' style phrases are also supported like
#' list(mean = ~mean(.x, na.rm = TRUE), sd = sd)
#'
#' @export
#'
#' @examples
#' compare_columns(flights_jfk, )
#' compare_columns(flights_jfk, ends_with("delay"))
compare_columns <- function(df,
                            ...,
                            calc = list(mean = mean)) {
  aggregate_group(df, "", ..., calc = calc)
}


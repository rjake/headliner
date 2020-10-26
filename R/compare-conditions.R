#' Compare two conditions within a data frame
#'
#' @param df data frame
#' @param compare condition for comparison, same criteria you would use in
#' 'dplyr::filter'
#' @param reference same as compare
#' @param cols columns to use in comparison
#' @param calc named list of the functions to use, ex:
#' list(mean = mean, sd = sd) 'purrr' style phrases are also supported like
#' list(mean = ~mean(.x, na.rm = TRUE), sd = sd)
#' @importFrom dplyr everything
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
#'   view_list()
#'
#' mtcars %>%
#'   compare_conditions(
#'     compare = (hp > 100 | gear == 4),
#'     reference = (mpg > 24),
#'     dplyr::starts_with("d"),
#'     calc = list(mean = mean, max = max)
#'   ) %>%
#'   view_list()
compare_conditions <- function(df,
                               compare,
                               reference,
                               cols = everything(),
                               calc = list(mean = mean)
                               ) {
  res_1 <- aggregate_group(df, "_comp", {{cols}}, calc = calc, cond = {{compare}})
  res_2 <- aggregate_group(df, "_ref", {{cols}}, calc = calc, cond = {{reference}})
  final <- append(res_1, res_2)
  final[order(names(final))]
}

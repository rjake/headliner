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
#' # compare_conditions() produces a list that can be passed to headline_list()
#' flights_jfk %>%
#'   compare_conditions(
#'     compare = (hour > 12),
#'     reference = (hour <= 12),
#'     dep_delay
#'   )
#'
#' .Last.value %>% headline_list()
#'
#'  # you can return multiple objects to compare
#'  # 'view_List()' is a helper to see list objects in a compact way
#' flights_jfk %>%
#'   compare_conditions(
#'     compare = (hour > 12),
#'     reference = (hour <= 12),
#'     cols = c(dep_delay, arr_delay),
#'     calc = list(mean = mean, sd = sd)
#'   ) %>%
#'   view_list()
#'
#' # if you want to compare to the average use 'complete.cases(.)'
#' flights_jfk %>%
#'   compare_conditions(
#'     compare = (hour > 12),
#'     reference = complete.cases(.),
#'     dep_delay
#'   ) %>%
#'   view_list()
compare_conditions <- function(df,
                               compare,
                               reference,
                               cols = everything(),
                               calc = list(mean = mean)
                               ) {
  res_1 <- aggregate_group(df, name = "_comp", cols = {{cols}}, calc = calc, cond = {{compare}})
  res_2 <- aggregate_group(df, name = "_ref",  cols = {{cols}}, calc = calc, cond = {{reference}})
  final <- append(res_1, res_2)
  final[order(names(final))]
}

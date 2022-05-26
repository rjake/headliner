#' Compare two conditions within a data frame
#'
#' @param df data frame
#' @param x condition for comparison, same criteria you would use in
#' 'dplyr::filter', used in contrast to the reference group 'y'
#' @param y condition for comparison, same criteria you would use in
#' 'dplyr::filter', used in contrast to the reference group 'x'
#' @param cols columns to use in comparison
#' @param calc named list of the functions to use, ex:
#' list(avg = mean, sd = sd) 'purrr' style phrases are also supported like
#' list(mean = ~mean(.x, na.rm = TRUE), sd = sd) and dplyr::lst(mean, sd) will
#' create a list(mean = mean, sd = sd)
#' @importFrom dplyr everything lst
#' @export
#'
#' @examples
#' # compare_conditions() produces a list that can be passed to headline_list()
#' flights_jfk |>
#'   compare_conditions(
#'     x = (hour > 12),
#'     y = (hour <= 12),
#'     dep_delay
#'   )
#'
#' .Last.value |> headline_list()
#'
#'  # you can return multiple objects to compare
#'  # 'view_List()' is a helper to see list objects in a compact way
#' flights_jfk |>
#'   compare_conditions(
#'     x = (hour > 12),
#'     y = (hour <= 12),
#'     cols = c(dep_delay, arr_delay),
#'     calc = list(mean = mean, sd = sd)
#'   ) |>
#'   view_list()
#'
#' # if you want to compare x to the overall average, use y = TRUE or if there
#' # may be NA values, you can pass anything that will return TRUE such as
#' # !is.na(dep_delay)
#' flights_jfk |>
#'   compare_conditions(
#'     x = (hour > 12),
#'     y = TRUE,
#'     cols = dep_delay
#'   ) |>
#'   view_list()
compare_conditions <- function(df,
                               x,
                               y,
                               cols = everything(),
                               calc = lst(mean)
                               ) {
  # sample inputs for debugging
    # df <- flights_jfk
    # x <- rlang::new_quosure(rlang::expr(hour > 12))
    # y <- rlang::new_quosure(rlang::expr(TRUE))
    # cols <- as.symbol("dep_delay")
    # calc <- dplyr::lst(mean)

  res_1 <- aggregate_group(df, name = "_x", cols = {{cols}}, calc = calc, cond = {{x}})
  res_2 <- aggregate_group(df, name = "_y",  cols = {{cols}}, calc = calc, cond = {{y}})

  final <- append(res_1, res_2)
  final[order(names(final))]
}

#' Compare two conditions within a data frame
#'
#' @param df data frame
#' @param x condition for comparison, same criteria you would use in
#' 'dplyr::filter', used in contrast to the reference group 'y'
#' @param y condition for comparison, same criteria you would use in
#' 'dplyr::filter', used in contrast to the reference group 'x'
#' @param .cols columns to use in comparison
#' @param .fns named list of the functions to use, ex:
#' list(avg = mean, sd = sd) 'purrr' style phrases are also supported like
#' list(mean = ~mean(.x, na.rm = TRUE), sd = sd) and dplyr::lst(mean, sd) will
#' create a list(mean = mean, sd = sd)
#' @importFrom dplyr everything lst group_vars group_keys
#' select left_join bind_cols relocate
#' @export
#'
#' @examples
#'
#' # compare_conditions works similar to dplyr::across()
#' flights_jfk |>
#'   compare_conditions(
#'     x = (hour > 12),
#'     y = (hour <= 12),
#'     .cols = dep_delay
#'   )
#'
#'
#' # because data frames are just fancy lists, you pass the result to headline_list()
#' flights_jfk |>
#'   compare_conditions(
#'     x = (hour > 12),
#'     y = (hour <= 12),
#'     .cols = dep_delay
#'   ) |>
#'   headline_list("a difference of {delta} minutes")
#'
#'
#'  # you can return multiple objects to compare
#'  # 'view_List()' is a helper to see list objects in a compact way
#' flights_jfk |>
#'   compare_conditions(
#'     x = (hour > 12),
#'     y = (hour <= 12),
#'     .cols = c(wind_dir, visib),
#'     .fns = dplyr::lst(mean, sd)
#'   ) |>
#'   view_list()
#'
#' # if you want to compare x to the overall average, use y = TRUE
#' flights_jfk |>
#'   compare_conditions(
#'     x = (hour > 12),
#'     y = TRUE,
#'     .cols = dep_delay
#'   )
compare_conditions <- function(df,
                               x,
                               y,
                               .cols = everything(),
                               .fns = lst(mean),
                               ...
                               ) {
  # sample inputs for debugging
    # df <- flights_jfk; .cols <- as.symbol("dep_delay"); .fns <- lst(mean, sd)
    # x <- rlang::new_quosure(rlang::expr(hour > 12))
    # y <- rlang::new_quosure(rlang::expr(TRUE))

  res_1 <- aggregate_group(df, name = "_x", .cols = {{.cols}}, .fns = .fns, cond = {{x}})
  res_2 <- aggregate_group(df, name = "_y", .cols = {{.cols}}, .fns = .fns, cond = {{y}})

  # need to account for grouped data frame & reorder vars so they are in order
  # ex: mean_x, mean_y, sd_x, sd_y
  any_groups <- group_vars(df)
  column_order <-
    c(
      any_groups,
      sort(c(names(res_1), names(res_2)))
    ) |>
    unique()


  if (length(any_groups)) { # has groups
    final <-
      group_keys(df) |>
      left_join(res_1) |>
      left_join(res_2) |>
      suppressMessages() # join msg

  } else { # no groups
    final <-
      res_1 |>
      bind_cols(res_2) |>
      suppressWarnings()
  }

  final |>
    relocate(column_order)
}

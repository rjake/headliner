#' Compare two conditions within a data frame
#'
#' @description Using logic that \code{\link[dplyr]{filter}} can interpret,
#' `compare_conditions()` will summarize the data aggregating condition x and
#' condition y
#'
#' @return a data frame that is either 1 row, or if grouped, 1 row per group
#'
#' @details `compare_conditions()` passes its arguments to
#' \code{\link[dplyr]{across}}. The `.cols` and `.fns` work the same. For
#' clarity, it is helpful to use the \code{\link[dplyr]{lst}} function for the
#' `.fns` parameter. Using
#' `compare_conditions(..., .cols = my_var, .fns = lst(mean, sd))` will return
#' the values `mean_my_var_x`, `mean_my_var_y`, `sd_my_var_x` and `sd_my_var_x`
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
#' pixar_films |>
#'   compare_conditions(
#'     x = (rating == "G"),
#'     y = (rating == "PG"),
#'     .cols = rotten_tomatoes
#'   )
#'
#'
#' # because data frames are just fancy lists, you pass the result to headline_list()
#' pixar_films |>
#'   compare_conditions(
#'     x = (rating == "G"),
#'     y = (rating == "PG"),
#'     .cols = rotten_tomatoes
#'   ) |>
#'   headline_list("a difference of {delta} points")
#'
#'
#'  # you can return multiple objects to compare
#'  # 'view_List()' is a helper to see list objects in a compact way
#'  pixar_films |>
#'   compare_conditions(
#'     x = (rating == "G"),
#'     y = (rating == "PG"),
#'     .cols = c(rotten_tomatoes, metacritic),
#'     .fns = dplyr::lst(mean, sd)
#'   ) |>
#'   view_list()
#'
#'
#' # if you want to compare x to the overall average, use y = TRUE
#' pixar_films |>
#'   compare_conditions(
#'     x = (rating == "G"),
#'     y = TRUE,
#'     .cols = rotten_tomatoes
#'   )
#'
#'
#' # to get the # of observations use length() instead of n()
#' pixar_films |>
#'   compare_conditions(
#'     x = (rating == "G"),
#'     y = (rating == "PG"),
#'     .cols = 1, # can put anything here really
#'     .fns = list(n = length)
#'   )
#'
#'
#' # you can also look at categorical data with functions like dplyr::n_distinct()
#' pixar_films |>
#'   compare_conditions(
#'     x = (rating == "G"),
#'     y = (rating == "PG"),
#'     .cols = film,
#'     .fns = list(distinct = dplyr::n_distinct)
#'   )
compare_conditions <- function(df,
                               x,
                               y,
                               .cols = everything(),
                               .fns = lst(mean)
                               ) {
  # sample inputs for debugging
    # df <- pixar_films; .cols <- as.symbol("rotten_tomatoes"); .fns <- lst(mean, sd)
    # x <- rlang::new_quosure(rlang::expr(rating == "G"))
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

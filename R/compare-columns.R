#' Compare two columns within a data frame
#'
#' @param df data frame
#' @param cols columns to compare, supports select helpers like 'starts_with()'
#' or 'where(is.numeric)'
#' @param calc named list of the functions to use, ex:
#' list(mean = mean, sd = sd) 'purrr' style phrases are also supported like
#' list(mean = ~mean(.x, na.rm = TRUE), sd = sd)
#' @importFrom dplyr everything
#' @importFrom glue glue
#' @export
#' @rdname compare_conditions
#'
#' @examples
#' # by default the function will compute the means
#' compare_columns(flights_jfk, c("dep_delay", "arr_delay"))
#'
#' # you can name the output using 'list()' and select the columns using
#' # any of the 'select()' helpers
#' compare_columns(flights_jfk, ends_with("delay"), list(avg = mean))
compare_columns <- function(df,
                            cols = everything(),
                            calc = list(mean = mean)) {
  res <- aggregate_group(df, name = "", cols = {{cols}}, calc = calc)
  res[order(names(res))]
}

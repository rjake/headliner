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
#'
#' @examples
#' compare_columns(flights_jfk, )
#' compare_columns(flights_jfk, ends_with("delay"))
compare_columns <- function(df,
                            cols = everything(),
                            calc = list(mean = mean)) {
  if (nrow(df) == 1) {
    warning(
      glue("There is only 1 row in the data frame. Did you mean to use \\
           compare_values() or headline()?"),
      call. = FALSE
    )
  }
  res <- aggregate_group(df, "", {{cols}}, calc = calc)
  res[order(names(res))]
}

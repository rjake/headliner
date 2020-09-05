#' Title
#'
#' @param df
#' @param group1
#' @param var
#' @param fun
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
compare_conditions <- function(df,
                               compare = complete.cases(.),
                               reference = complete.cases(.),
                               ...,
                               calc = list(mean = mean),
                               cond = NULL) {
  res_1 <- aggregate_group(df, "comp_", ..., calc = calc, cond = {{compare}})
  res_2 <- aggregate_group(df, "ref_", ..., calc = calc, cond = {{reference}})
  append(res_1, res_2)
}


compare_columns <- function(df,
                            ...,
                            calc = list(mean = mean)) {
  aggregate_group(df, "ref_", ..., calc = calc)
}


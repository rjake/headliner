library(purrr)

keep_numeric <- function(x) {
  x %>%
    map(keep, is.numeric) %>%
    compact() %>%
    as.numeric()
}


test_that("compare_values produces list", {
  x <- compare_values(2, 10)

  expect_true(is.list(x))

  expect_true(x$delta == 8)
  expect_true(x$raw_delta == -8)
  expect_true(x$article_delta == "an 8")
  expect_true(x$article_raw_delta == "a -8")

  expect_true(
    all(
      names(x) == c(
        "delta", "delta_p",
        "article_delta",  "article_delta_p",
        "comp_value", "ref_value",
        "raw_delta", "raw_delta_p",
        "article_raw_delta", "article_raw_delta_p",
        "sign", "orig_values", "trend"
      )
    )
  )
})


test_that("multiplier works", {
  select_vars <- function(x) {
    view_list(x)[c("delta", "raw_delta", "comp_value", "ref_value"), 1] %>%
      as.numeric()
  }

  whole_numbers <- select_vars(compare_values(23.4, 34.5))
  multiplied <- select_vars(compare_values(0.234, 0.345, multiplier = 100))
  decimal <- select_vars(compare_values(0.234, 0.345, n_decimal = 2))

  expect_equal(whole_numbers, multiplied)
  expect_equal(round(multiplied / 100, 2), decimal)
})

test_that("check rounding runs", {
  expect_warning(compare_values(0.123, 0.12))
  expect_warning(compare_values(21.1, 21.1))
  # no wawning
  expect_warning(compare_values(0.123, 0.1234, n_decimal = 4), regexp = NA)
  expect_warning(compare_values(0.12, 0.123, multiplier = 100), regexp = NA)
})


test_that("compare_values accepts multiple trend terms", {
  use_values <- c("increase", "higher")

  x <-
    compare_values(
      5, 4,
      trend_phrases = trend_terms(more = use_values)
    )

  expect_equal(x$trend, use_values)
  expect_equal(x$article_trend, paste(c("an", "a"), use_values))
})

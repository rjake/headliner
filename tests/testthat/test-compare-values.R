library(purrr)

keep_numeric <- function(x) {
  x %>%
    map(keep, is.numeric) %>%
    compact() %>%
    as.numeric()
}


test_that("compare_values produces list", {
  x <- compare_values(2, 3)

  expect_true(is.list(x))

  expect_true(x$delta == 1)
  expect_true(x$raw_delta == -1)

  expect_true(
    all(
      names(x) == c(
        "delta", "trend", "delta_p", "article_delta",  "article_delta_p",
        "article_trend", "comp_value", "ref_value", "raw_delta", "raw_delta_p",
        "sign", "orig_values"
      )
    )
  )
})


test_that("multiplier works", {
  whole_numbers <- keep_numeric(compare_values(20, 30))
  multiplierd <- keep_numeric(compare_values(0.2, 0.3, multiplier = 100))
  decimal <- keep_numeric(compare_values(0.2, 0.3, n_decimal = 2))

  expect_equal(whole_numbers[1:2], multiplierd[1:2])
  expect_equal(multiplierd[1] / 100, decimal[1])
})

test_that("check rounding runs", {
  expect_warning(compare_values(0.123, 0.12))
  expect_warning(compare_values(21.1, 21.1))
  # no wawning
  expect_warning(compare_values(0.123, 0.1234, n_decimal = 4), regexp = NA)
})

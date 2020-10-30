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


test_that("rescale works", {
  whole_numbers <- keep_numeric(compare_values(20, 25))
  rescaled <- keep_numeric(compare_values(0.2, 0.25, rescale = 100))
  decimal <- keep_numeric(compare_values(0.2, 0.25, n_decimal = 2))

  expect_equal(whole_numbers[1:2], rescaled[1:2])
  expect_equal(rescaled[1] / 100, decimal[1])
})

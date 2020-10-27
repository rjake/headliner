test_that("compare_values produces list", {
  x <- compare_values(c(2, 3))

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


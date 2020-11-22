test_that("view_list works", {
  x <- compare_values(10, 8)
  x_df <- view_list(x)

  expect_true(inherits(x_df, "data.frame"))

  expect_equal(
    length(x),
    nrow(view_list(x))
  )

  expect_equal(
    x$orig_values,
    x_df["orig_values", "VALUES"]
  )
})

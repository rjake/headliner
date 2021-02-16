test_that("add headline column works", {
  df <- add_headline_column(mtcars, gear, carb)

  df_new_col <- add_headline_column(mtcars, gear, carb, .name = "abc")

  expect_true("headline" %in% names(df))
  expect_true("abc" %in% names(df_new_col))
  expect_warning(
    add_headline_column(mtcars, gear, carb, .name = "mpg")
  )
})

test_that("add headline column returns columns", {
  df <-
    add_headline_column(
      df = mtcars,
      compare = gear,
      reference = carb,
      return_cols = dplyr::starts_with("delta")
    )

  expect_true("headline" %in% names(df))
  expect_true(all(c("delta", "delta_p") %in% names(df)))
})
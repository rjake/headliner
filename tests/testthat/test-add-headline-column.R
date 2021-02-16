test_that("add headline column works", {
  df <- add_headline_column(mtcars, gear, carb)

  df_new_col <- add_headline_column(mtcars, gear, carb, .name = "abc")

  expect_true("headline" %in% names(df))
  expect_true("abc" %in% names(df_new_col))
  expect_warning(
    add_headline_column(mtcars, gear, carb, .name = "mpg")
  )
})

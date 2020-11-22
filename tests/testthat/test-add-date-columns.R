library(lubridate)

test_that("add_date_columns works", {
  x <- ymd(20210101)
  df <-
    tibble::tibble(
      date = x %m+% months(-7:9)
    ) %>%
    add_date_columns(date, ref_date = ymd(20210101))

  expect_equal(dim(df), c(17, 7))
  expect_equal(range(df$day), c(-214, 273))
  expect_equal(range(df$week), c(-30, 39))
  expect_equal(range(df$month), c(-7, 9))
  expect_equal(range(df$quarter), c(-3, 3))
  expect_equal(range(df$calendar_year), c(-1, 0))
  expect_equal(range(df$fiscal_year), c(-1, 1))

  df_positive <-
    df %>%
    dplyr::mutate_if(is.numeric, ~(.x > 0))

  expect_equal(sum(df_positive$day), 9)
  expect_equal(sum(df_positive$week), 9)
  expect_equal(sum(df_positive$month), 9)
  expect_equal(sum(df_positive$quarter), 7)
  expect_equal(sum(df_positive$calendar_year), 0)
  expect_equal(sum(df_positive$fiscal_year), 4)
})

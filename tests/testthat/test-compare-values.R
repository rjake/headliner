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
        "x", "y",
        "delta", "delta_p",
        "article_delta",  "article_delta_p",
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
  expect_message(compare_values(0.123, 0.12))
  expect_message(compare_values(21.1, 21.12))
  # no message
  expect_warning(compare_values(0.123, 0.1234, n_decimal = 4), regexp = NA)
  expect_warning(compare_values(0.12, 0.123, multiplier = 100), regexp = NA)
})


test_that("compare_values accepts multiple trend terms", {
  trend_list <-
    list(
      when_more = trend_terms("more", "less"),
      when_higher = trend_terms("higher", "lower")
    )

  x <-
    compare_values(
      5, 4,
      trend_phrases = trend_list
    )

  expect_equal(x$when_more, "more")
  expect_equal(x$when_higher, "higher")
})


test_that("compare_values accepts multiple trend terms", {
  plural_list <-
    list(
      n_people = plural_phrasing("person", "people"),
      n_cats = plural_phrasing("cat", "cats")
    )

  x <-
    compare_values(
      5, 4,
      plural_phrases = plural_list
    )

  expect_equal(x$n_people, "person")
  expect_equal(x$n_cats, "cat")
})


test_that("return_trend_phrases() works", {
  test_direct <- trend_terms()
  test_list <- list(trend = trend_terms())

  expect_equal(
    return_trend_phrases(test_direct),
    return_trend_phrases(test_list)
  )
})

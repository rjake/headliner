test_that("check_overlapping_names() works", {
  expect_warning(
    object = check_overlapping_names(mtcars, mtcars[,1:3]),
    regexp = "duplicate names"
  )

  # no warning/no overlap
  expect_warning(
    object = check_overlapping_names(mtcars, iris),
    regexp = NA
  )
})


test_that("aggregate_group() works", {
  x <-
    aggregate_group(
      df = mtcars,
      name = "_x",
      .cols = c(mpg, hp),
      .fns = list(avg = mean)
    )

  expect_equal(names(x), c("avg_mpg_x", "avg_hp_x"))
  expect_equal(x$avg_mpg_x, mean(mtcars$mpg))
  expect_equal(x$avg_hp_x, mean(mtcars$hp))
})


test_that("aggregate_group() allows grouped df", {
  x <-
    aggregate_group(
      df = dplyr::group_by(mtcars, cyl),
      name = "_x",
      .cols = mpg,
      .fns = mean
    )

  expect_equal(dim(x), c(3, 2))
})


test_that("check rounding throws a message", {
  expect_null(check_rounding(0.2, 0.24, n_decimal = 2))
  expect_null(check_rounding(0.21, 0.21, n_decimal = 1))
  expect_message(check_rounding(0.2, 0.24, n_decimal = 1))
  expect_message(
    check_rounding(x = c(1.9, 2.01), y = 2, n_decimal = 1),
    regexp = "input #2)"
  )
  expect_message(
    check_rounding(x = c(2.01, 2.02, 2.1), y = 2, n_decimal = 1),
    regexp = "input #1 and 2)"
  )
  # expect_message(
  #   data.frame(new = 18:22/100, old = 0.2) |>
  #     add_headline_column(
  #       new, old,
  #       return_cols = c(x, y)
  #     ),
  #   regexp = "input #1 and 2)"
  # )
})


test_that("get_article() works for characters", {
  expect_equal(get_article("decrease"), "a")
  expect_equal(get_article("increase"), "an")
})



test_that("get_article() works for numbers", {
  is_a <-
    purrr::map_chr(
      c(-8, -6, -0.2, 0.123, 0:7, 9, 10, 12:17, 19:20, 199, 1234, 1000111, 12000111),
      get_article
    )

  is_an <-
    purrr::map_chr(
      c(8, 80, 11, 18, 11800, 11000, 11000111),
      get_article
    )

  expect_equal(unique(is_a), "a")
  expect_equal(unique(is_an), "an")

})


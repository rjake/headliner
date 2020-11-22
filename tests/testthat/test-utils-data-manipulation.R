test_that("aggregate_group() works", {
  x <-
    aggregate_group(
      df = mtcars,
      name = "_comp",
      .col = c(mpg, hp),
      calc = list(avg = mean)
    )

  expect_equal(names(x), c("avg_mpg_comp", "avg_hp_comp"))
  expect_equal(x$avg_mpg_comp, mean(mtcars$mpg))
  expect_equal(x$avg_hp_comp, mean(mtcars$hp))
})


test_that("check rounding throws warnings", {
  expect_warning(check_rounding(0.2, 0.24, n_decimal = 1))
  expect_null(check_rounding(0.2, 0.24, n_decimal = 2))
  expect_null(check_rounding(21, 21, n_decimal = 1))
})



test_that("get_article() works for characters", {
  expect_equal(get_article("decrease"), "a")
  expect_equal(get_article("increase"), "an")
})



test_that("get_article() works for numbers", {
  is_a <-
    purrr::map_chr(
      c(0:7, 9, 10, 12:20, 199, 1234, 1000111, 12000111),
      get_article
    )

  is_an <-
    purrr::map_chr(
      c(8, 80, 11, 11000, 11000111),
      get_article
    )

  expect_equal(unique(is_a), "a")
  expect_equal(unique(is_an), "an")

})

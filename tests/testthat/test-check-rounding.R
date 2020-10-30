test_that("check rounding throws warnings", {
  expect_warning(check_rounding(0.2, 0.24, n_decimal = 1))
  expect_null(check_rounding(0.2, 0.24, n_decimal = 2))
  expect_null(check_rounding(21, 21, n_decimal = 1))
})

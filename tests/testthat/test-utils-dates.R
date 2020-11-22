library(lubridate)

test_that("calc_distance works", {
  expect_equal(
    calc_distance(ymd(20210101) + (-3:3), "days", ymd(20210101)),
    -3:3
  )
  expect_equal(
    calc_distance(ymd(20210115) %m+% months(-3:3), "months", ymd(20210101)),
    -3:3
  )
})

test_that("fiscal_date works", {
  expect_equal(fiscal_date(as.Date("2021-01-01"), offset = 0), ymd(20210101))
  expect_equal(fiscal_date(as.Date("2021-01-01"), offset = 6), ymd(20210701))
})

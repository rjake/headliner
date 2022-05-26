test_that("demo_data works", {
  x <- demo_data()
  n <- nrow(x)

  min_month <- min(seq.Date(Sys.Date(), length.out = n, by = "-2 month"))

  expect_equal(x$date[1], Sys.Date())
  expect_equal(x$date[n], min_month)
})


test_that("demo_data accepts params", {
  x <- demo_data(n = 2, by = "-1 day")

  expect_equal(x$date[1], Sys.Date())
  expect_equal(x$date[2], Sys.Date() - 1)
})


test_that("large n works", {
  (demo_data(n = 6)$group == rep(letters[1:3], each = 2)) |>
    all() |>
    expect_true()

  (unique(demo_data(n = 200)$group) == letters) |>
    all() |>
    expect_true()
})

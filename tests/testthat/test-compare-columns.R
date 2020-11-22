test_that("mean (default) works", {
  x_mean <- compare_columns(flights_jfk, ends_with("delay"))
  expect_equal(names(x_mean), c("mean_arr_delay", "mean_dep_delay"))
  expect_equal(x_mean$mean_arr_delay, mean(flights_jfk$arr_delay))
  expect_equal(x_mean$mean_dep_delay, mean(flights_jfk$dep_delay))
})

test_that("max works", {
  x_max <-
    compare_columns(
      df = flights_jfk,
      cols = ends_with("delay"),
      calc = list(max = max)
    )
  expect_equal(names(x_max), c("max_arr_delay", "max_dep_delay"))
  expect_equal(x_max$max_arr_delay, max(flights_jfk$arr_delay))
  expect_equal(x_max$max_dep_delay, max(flights_jfk$dep_delay))
})

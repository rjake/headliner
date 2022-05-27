test_that("mean (default) works", {
  x <-
    flights_jfk |>
    compare_conditions(
      x = (carrier == "DL"),
      y = TRUE,
      dep_delay
    )

  expect_equal(names(x), c("mean_dep_delay_x", "mean_dep_delay_y"))
  expect_equal(
    x$mean_dep_delay_x,
    mean(flights_jfk$dep_delay[flights_jfk$carrier == "DL"])
  )

  expect_equal(
    x$mean_dep_delay_y,
    mean(flights_jfk$dep_delay)
  )
})


test_that("max works", {
  x <-
    flights_jfk |>
    compare_conditions(
      x = (carrier == "DL"),
      y = TRUE,
      .cols = dep_delay,
      .fns = list(max = max)
    )

  expect_equal(names(x), c("max_dep_delay_x", "max_dep_delay_y"))
  expect_equal(
    x$max_dep_delay_x,
    max(flights_jfk$dep_delay[flights_jfk$carrier == "DL"])
  )

  expect_equal(
    x$max_dep_delay_y,
    max(flights_jfk$dep_delay)
  )
})


test_that("grouped df vs non-grouped", {
  df <-
    compare_conditions(
      df = mtcars,
      .cols = mpg,
      .fns = mean
    )

  df_grouped <-
    compare_conditions(
      df = dplyr::group_by(mtcars, cyl),
      .cols = mpg,
      .fns = mean
    )

  expect_equal(dim(df), c(1, 2))
  expect_equal(dim(df_grouped), c(3, 3))
})

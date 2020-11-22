test_that("mean (default) works", {
  x <-
    flights_jfk %>%
    compare_conditions(
      compare = (carrier == "DL"),
      reference = complete.cases(.),
      dep_delay
    )

  expect_equal(names(x), c("mean_dep_delay_comp", "mean_dep_delay_ref"))
  expect_equal(
    x$mean_dep_delay_comp,
    mean(flights_jfk$dep_delay[flights_jfk$carrier == "DL"])
  )

  expect_equal(
    x$mean_dep_delay_ref,
    mean(flights_jfk$dep_delay)
  )
})


test_that("max works", {
  x <-
    flights_jfk %>%
    compare_conditions(
      compare = (carrier == "DL"),
      reference = complete.cases(.),
      cols = dep_delay,
      calc = list(max = max)
    )

  expect_equal(names(x), c("max_dep_delay_comp", "max_dep_delay_ref"))
  expect_equal(
    x$max_dep_delay_comp,
    max(flights_jfk$dep_delay[flights_jfk$carrier == "DL"])
  )

  expect_equal(
    x$max_dep_delay_ref,
    max(flights_jfk$dep_delay)
  )
})

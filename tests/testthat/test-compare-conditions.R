test_that("mean (default) works", {
  x <-
    pixar_films |>
    compare_conditions(
      x = (rating == "G"),
      y = TRUE,
      bo_domestic
    )

  expect_equal(names(x), c("mean_bo_domestic_x", "mean_bo_domestic_y"))
  expect_equal(
    x$mean_bo_domestic_x,
    mean(pixar_films$bo_domestic[pixar_films$rating == "G"])
  )

  expect_equal(
    x$mean_bo_domestic_y,
    mean(pixar_films$bo_domestic)
  )
})


test_that("max works", {
  x <-
    pixar_films |>
    compare_conditions(
      x = (rating == "G"),
      y = TRUE,
      .cols = bo_domestic,
      .fns = list(max = max)
    )

  expect_equal(names(x), c("max_bo_domestic_x", "max_bo_domestic_y"))
  expect_equal(
    x$max_bo_domestic_x,
    max(pixar_films$bo_domestic[pixar_films$rating == "G"])
  )

  expect_equal(
    x$max_bo_domestic_y,
    max(pixar_films$bo_domestic)
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

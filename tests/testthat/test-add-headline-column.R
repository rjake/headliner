test_that("add headline column works", {
  df <- add_headline_column(mtcars, gear, carb)

  df_new_col <- add_headline_column(mtcars, gear, carb, .name = "abc")

  expect_true("headline" %in% names(df))
  expect_true("abc" %in% names(df_new_col))
  expect_warning(
    add_headline_column(mtcars, gear, carb, .name = "mpg")
  )
})

test_that("add headline column returns columns", {
  df <-
    add_headline_column(
      df = mtcars,
      x = gear,
      y = carb,
      return_cols = dplyr::starts_with("delta")
    )

  expect_true("headline" %in% names(df))
  expect_true(all(c("delta", "delta_p") %in% names(df)))
})


test_that("add headline can access other columns", {
  df <-
    add_headline_column(
      df = animal_sleep,
      x = hours_asleep,
      y = hours_awake,
      headline = "{common_name} ({orig_values})"
    ) %>%
    mutate(has_text = purrr::map2_lgl(common_name, headline, grepl))

  expect_true(all(df$has_text))
})


test_that("add headline can pas '...", {
  df <-
    add_headline_column(
      df = animal_sleep,
      x = hours_asleep,
      y = hours_awake,
      headline = "{abc} {common_name} {trend}",
      abc = "123"
    )  %>%
    mutate(has_text = purrr::map2_lgl("^123", headline, grepl))

  expect_true(all(df$has_text))
})

test_that("if_match works", {
  df <-
    data.frame(
      a = 1:3,
      b = 3:1
    ) %>%
    add_headline_column(a, b)

  expect_equal(
    df$headline[2],
    formals(add_headline_column)[["if_match"]]
  )
})

test_that("error if x or y in column names", {
  data.frame(
    x = 1:3,
    y = 3:1
  ) %>%
  add_headline_column(x, y) |>
  expect_error()
})

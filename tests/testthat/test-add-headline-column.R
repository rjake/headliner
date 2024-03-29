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
      df = pixar_films,
      x = bo_domestic,
      y = bo_intl,
      headline = "{film} ({orig_values})"
    ) |>
    mutate(has_text = purrr::map2_lgl(film, headline, grepl))

  expect_true(all(df$has_text))
})


test_that("add headline can pass '...", {
  df <-
    add_headline_column(
      df = pixar_films,
      x = bo_domestic,
      y = bo_intl,
      headline = "{abc} {film} {trend}",
      abc = "123"
    )  |>
    mutate(has_text = purrr::map2_lgl("^123", headline, grepl))

  expect_true(all(df$has_text))
})

test_that("if_match works", {
  df <-
    data.frame(
      a = 1:3,
      b = 3:1
    ) |>
    add_headline_column(a, b)

  expect_equal(
    df$headline[2],
    formals(add_headline_column)[["if_match"]]
  )
})

test_that("warning if columns renamed", {
  data.frame(
    x = 1:3,
    y = 3:1
  ) |>
  add_headline_column(
    x = y,
    y = x,
    return_cols = everything()
  ) |>
  expect_message("New names")
})


test_that("returns NA instead of error", {
  expect_error(
    object = {
      tibble(x = 1:4, y = c(3:1, NA)) |>
        add_headline_column(x, y)

    },
    regexp = NA # no error
  )
})

test_that("uses dynamic headline", {
  headlines <- c(
    "{other} has {trend}d",
    "{delta_p}"
  )

  df <-
    data.frame(
      a = 1:2,
      b = 2:3,
      other = "car"
    ) |>
    mutate(phrase = headlines)

  res <-
    df |>
    add_headline_column(a, b, phrase)

  expect_equal(
    res$headline,
    c("car has decreased", "33.3")
  )
})

test_that("lists passed correctly", {
  res <-
    data.frame(
      a = c(2, 5),
      b = 3
    ) |>
    add_headline_column(
      a, b,
      headline = "{delta} {more} {cat} ({higher})",
      trend_phrases = list(
        higher = trend_terms("higher", "lower"),
        more = trend_terms("more", "less")
      ),
      plural_phrases = list(
        cat = plural_phrasing("cat", "cats")
      )
    )
  expect_equal(
    res$headline,
    c("1 less cat (lower)", "2 more cats (higher)")
  )
})

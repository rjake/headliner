test_that("vector works correctly", {
  expect_equal(
    headline(1, 3),
    "decrease of 2 (1 vs. 3)"
  )
})


test_that("list is passed correctly", {
  named <-
    list(a = 8, b = 9, c = 10) |>
    headline_list(x = c, y = a)

  unnamed <-
    list(10, 8) |>
    headline_list()

  phrase <- glue::glue("increase of 2 (10 vs. 8)")

  expect_equal(phrase, named)
  expect_equal(phrase, unnamed)
})

test_that("check that list with multiple elements shows message", {
  expect_message(
    data.frame(a = 2, b = 3, c = 4) |>
      headline_list(),
    regexp = "two elements"
  )
})

test_that("phrases added", {
  phrases <- list(
    person = plural_phrasing("person", "people"),
    was = plural_phrasing("was", "were")
  )
  when_multi <- headline(3, 1, plural_phrases = phrases, "{person} {was}")
  when_single <- headline(2, 1, plural_phrases = phrases, "{person} {was}")

  expect_true(when_multi == "people were")
  expect_true(when_single == "person was")
})



test_that("list is returned", {
  x <- headline(1, 2, return_data = TRUE)

  expect_true(inherits(x, "list"))
})


test_that("glue_data() allows extra expressions", {
  as_headline <-
    headline(
      3, 2,
      "{trend} on {date}",
      date = "1/1/2022"
    )

  as_list <-
    headline(
      3, 2,
      "{trend} on {date}",
      date = "1/1/2022",
      return_data = TRUE
    )

  expect_true(
    all(
      c(as_headline, as_list$headline) == "increase on 1/1/2022"
    )
  )
})


test_that("is vectorized", {
  expect_equal(
    object =
      headline(1:3, 3:1) |>
      unique() |>
      length(),
    expected = 3
  )
})


test_that("returns NA instead of error", {
  expect_error(
    object = headline(c(NA, 1:2), 3:1),
    regexp = NA # no error
  )
})


test_that("defaults are the same across similar functions", {
  hl <- formals(headline)
  cv <- formals(compare_values)
  hc <- formals(add_headline_column)

  hl_cv <- intersect(names(hl), names(cv))
  hl_hc <- intersect(names(hl), names(hc))

  expect_equal(
    hl[hl_cv],
    cv[hl_cv]
  )

  expect_equal(
    hl[hl_hc],
    hc[hl_hc]
  )
})

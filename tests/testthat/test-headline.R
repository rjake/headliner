
test_that("data frame is passed correctly", {
  named <-
    head(iris, 1) %>%
    headline(Sepal.Length, Sepal.Width)

  unnamed <-
    iris[1, 1:2] %>%
    headline()

  phrase <- glue::glue("increase of 1.6 (5.1 vs. 3.5)")

  expect_equal(phrase, named)
  expect_equal(phrase, unnamed)
})


test_that("list is passed correctly", {
  named <-
    list(a = 8, b = 9, c = 10) %>%
    headline(c, a)

  unnamed <-
    list(10, 8) %>%
    headline()

  phrase <- glue::glue("increase of 2 (10 vs. 8)")

  expect_equal(phrase, named)
  expect_equal(phrase, unnamed)
})



test_that("stop if # elements > 2 & unnamed", {
  expect_error(headline(iris[1, 1:3]), "Not sure")
  expect_error(headline(list(10, 9, 8)), "Not sure")
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

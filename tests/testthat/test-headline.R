test_that("vector works correctly", {
  expect_equal(
    headline(1, 3),
    "decrease of 2 (1 vs. 3)"
  )
})


test_that("list is passed correctly", {
  named <-
    list(a = 8, b = 9, c = 10) %>%
    headline_list(compare = c, reference = a)

  unnamed <-
    list(10, 8) %>%
    headline_list()

  phrase <- glue::glue("increase of 2 (10 vs. 8)")

  expect_equal(phrase, named)
  expect_equal(phrase, unnamed)
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


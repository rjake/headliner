test_that("plural_phrases works", {
  expect_error(
    compare_values(10, 2, plural_phrases = list(plural_phrasing("a", "an"))),
    "named list"
  )

  phrases <- list(
    person = plural_phrasing("person", "people"),
    was = plural_phrasing("was", "were")
  )

  when_multi <- compare_values(3, 1, plural_phrases = phrases)
  when_single <- compare_values(2, 1, plural_phrases = phrases)

  expect_true(when_multi$person == "people")
  expect_true(when_single$person == "person")

  expect_true(when_multi$was == "were")
  expect_true(when_single$was == "was")
})


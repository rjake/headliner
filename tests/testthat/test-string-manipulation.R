test_that("add_article() works", {
  expect_equal(add_article("increase"), "an increase")
  expect_equal(add_article("decrease"), "a decrease")
  expect_equal(add_article(11), "an 11")

  expect_equal(
    headline(3, 2, "there was {add_article(trend)}"),
    "there was an increase"
  )
})


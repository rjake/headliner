test_that("headline uses new defaults", {
  orig_headline <- headline(3, 4)

  # set custom headline
  pct_phrase <- "{delta_p}% {trend} ({orig_values})"
  orig_phrase <- "was {r} now {c}"
  set_headliner_defaults(
    headline = pct_phrase,
    orig_values = orig_phrase
  )
  expect_equal(headliner_global$headline, pct_phrase)
  expect_equal(headliner_global$orig_values, orig_phrase)

  expect_equal(
    object = headline(3, 4),
    expected = "25% decrease (was 4 now 3)"
  )

  expect_equal(
    object = compare_values(3, 4)$orig_values,
    expected = "was 4 now 3"
  )

  # clear custom headline
  set_headliner_defaults(headline = NULL, orig_values = NULL)
  expect_equal(headliner_global$headline, default_headline)
  expect_equal(headliner_global$orig_values, default_orig_values)

  expect_equal(
    object = headline(3, 4),
    expected = orig_headline
  )

  expect_equal(
    object = compare_values(3, 4)$orig_values,
    expected = "3 vs. 4"
  )

})


test_that("augment_article_patterns updates values", {
  # orig values
  expect_equal(
    object = get_article(c("universe", "hour")),
    expected = c("an", "a")
  )

  # updated values
  augment_article_patterns(
    regex_for_a = "univ",
    regex_for_an = "hour"
  )

  expect_equal(
    object = get_article(c("universe", "hour")),
    expected = c("a", "an")
  )

  # reset value
  augment_article_patterns(regex_for_a = NULL, regex_for_an = NULL)

  expect_equal(
    object = get_article(c("universe", "hour")),
    expected = c("an", "a")
  )

})


test_that("augment_article_patterns throws error", {
  # can only take a single string, not a vector
  expect_error(
    object = augment_article_patterns(regex_for_a = c("abc", "def")),
    regexp = "single regular expression"
  )
})

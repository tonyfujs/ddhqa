context("Other utils funcs")

# Define constant values

# Test is_blank
test_that("Handles blanks correctly", {
  expect_equal(is_blank(""), TRUE)
  expect_equal(is_blank(NA), TRUE)
  expect_equal(is_blank(NULL), TRUE)
  expect_equal(is_blank(c()), TRUE)
  expect_equal(is_blank(list()), TRUE)
})

test_that("Handles populated values correctly", {
  expect_equal(is_blank("train in vain"), FALSE)
  expect_equal(is_blank(c("train", "in", "vain")), FALSE)
  expect_equal(is_blank(list("train", "in", "vain")), FALSE)
})

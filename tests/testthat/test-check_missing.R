context("Tests check for datasets missing resources")

test_that("Catches single resource", {
  metadata <- list("nid" = "007", "field_resources" = 1975)
  expect_equal(
    check_missing(metadata),
    list("dataset", "007", "check_missing", "PASS", "has resources")
  )
})

test_that("Catches multiple resources", {
  metadata <- list("nid" = "007", "field_resources" = c(4, 4, 4))
  expect_equal(
    check_missing(metadata),
    list("dataset", "007", "check_missing", "PASS", "has resources")
  )
})

test_that("Catches missing resources", {
  metadata <- list("nid" = "007", "field_resources" = list())
  expect_equal(
    check_missing(metadata),
    list("dataset", "007", "check_missing", "FAIL", "needs resources")
  )
})

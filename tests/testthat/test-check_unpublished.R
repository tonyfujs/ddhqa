context("Tests check for unpublished resources")

test_that("Resource is published", {
  metadata <- list("field_dataset_ref" = "1618", "nid" = "007", "status" = 1)
  expect_equal(
    check_unpublished(metadata),
    list("resource", "007", "check_unpublished", "PASS", "published")
  )
})

test_that("Resource is not published", {
  metadata <- list("field_dataset_ref" = "1618", "nid" = "007", "status" = 0)
  expect_equal(
    check_unpublished(metadata),
    list("resource", "007", "check_unpublished", "FAIL", glue("unpublished resource for dataset 1618"))
  )
})

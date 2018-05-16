context("Tests check resource links")

test_that("Confirms valid status code", {
  metadata <- list("nid" = "007", "type" = "resource")
  metadata$field_link_remote_file$und[[1]]$url <- "datacatalog.worldbank.org"
  expect_equal(
    check_resource_link(metadata),
    c("resource", "007", "check_resource_links", "PASS", glue("200, you're good to go"))
  )
})

test_that("Confirms non-existent link", {
  metadata <- list("nid" = "007", "type" = "resource")
  metadata$field_link_remote_file$und[[1]]$url <- "bearsarecool.org"
  expect_equal(
    check_resource_link(metadata),
    c("resource", "007", resource_nid, "check_resource_links", "FAIL", glue("No response, check the link"))
  )
})

# test_that("Confirms 404 links", {
#   metadata <- list("nid" = "007", "type" = "resource")
#   metadata$field_link_remote_file$und[[1]]$url <- "datacatalog.worldbank.org/bears"
#   expect_equal(
#     check_resource_link(metadata),
#     c("resource", resource_nid, "check_resource_links", "FAIL", glue("No response, check the link"))
#   )
# })

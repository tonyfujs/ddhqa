context("Tests check resource links")

test_that("Confirms valid status code", {
  metadata <- list("nid" = "007", "type" = "resource")
  metadata$field_link_remote_file$und[[1]]$url <- "datacatalog.worldbank.org"
  expect_equal(
    check_resource_link(metadata),
    list("resource", "007", "check_resource_links", "PASS", glue::glue("200, ", "you're good to go"))
  )
})

test_that("Confirms non-existent link", {
  metadata <- list("nid" = "007", "type" = "resource")
  metadata$field_link_remote_file$und[[1]]$url <- "bearsarecool.org"
  expect_equal(
    check_resource_link(metadata),
    list("resource", "007", "check_resource_links", "FAIL", glue::glue("No response, ", "check the link"))
  )
})

test_that("Confirms 404 links", {
  metadata <- list("nid" = "007", "type" = "resource")
  metadata$field_link_remote_file$und[[1]]$url <- "http://geowb.worldbank.org/bears"
  expect_equal(
    check_resource_link(metadata),
    list("resource", "007", "check_resource_links", "PASS", glue::glue("404, ", "esri link, you're good to go"))
  )
})

test_that("Catch Confindential", {
  metadata <- list("nid" = "007", "field_wbddh_data_class" = "360")
  metadata$field_link_remote_file$und[[1]]$url <- ""
  expect_equal(
    check_resource_link(metadata),
    list("resource", "007", "check_resource_links", "PASS", glue::glue("No response,", "confidential resource, you're good to go"))
  )

})

test_that("Catch Strict Confindential", {
  metadata <- list("nid" = "007", "field_wbddh_data_class" = "361")
  metadata$field_link_remote_file$und[[1]]$url <- ""
  expect_equal(
    check_resource_link(metadata),
    list("resource", "007", "check_resource_links", "PASS", glue::glue("No response,", "confidential resource, you're good to go"))
  )

})

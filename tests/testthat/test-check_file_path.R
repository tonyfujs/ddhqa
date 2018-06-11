context("Tests check for resources' file path")

test_that("Resource is public but the path is not", {
  metadata <- list("nid" = "007", "field_wbddh_data_class" = "358")
  metadata$field_upload$und[[1]]$uri <- "public://dataset_resources/ddhfiles/internal/bears.csv"
  expect_equal(
    check_file_path(metadata),
    list("resource", "007", "check_file_path", "FAIL", "resource is public but file path is internal")
  )
})

test_that("Resource is public and path is public", {
  metadata <- list("nid" = "007", "field_wbddh_data_class" = "358")
  metadata$field_upload$und[[1]]$uri <- "public://dataset_resources/ddhfiles/public/bears.csv"
  expect_equal(
    check_file_path(metadata),
    list("resource", "007", "check_file_path", "PASS", "resource is public and file path is public")
  )
})

test_that("Resource is not public but path is public", {
  metadata <- list("nid" = "007", "field_wbddh_data_class" = "359")
  metadata$field_upload$und[[1]]$uri <- "public://dataset_resources/ddhfiles/public/bears.csv"
  expect_equal(
    check_file_path(metadata),
    list("resource", "007", "check_file_path", "FAIL", "resource is not public but file path is public")
  )
})

test_that("Resource is not public and path is not public", {
  metadata <- list("nid" = "007", "field_wbddh_data_class" = "359")
  metadata$field_upload$und[[1]]$uri <- "public://dataset_resources/ddhfiles/internal/bears.csv"
  expect_equal(
    check_file_path(metadata),
    list("resource", "007", "check_file_path", "FAIL", "resource is internal and file path is internal")
  )
})

test_that("Resource is not uploaded", {
  metadata <- list("nid" = "007", "field_wbddh_data_class" = "359")
  expect_equal(
    check_file_path(metadata),
    list("resource", "007", "check_file_path", "PASS", "resource is not uploaded")
  )
})

context("Tests for functions check privacy status")

httptest::start_capturing(path = "./tests/testthat")
dkanr::dkanr_setup(
  url = "https://datacatalog.worldbank.org/",
  username = Sys.getenv("ddh_username"),
  password = Sys.getenv("ddh_password")
)
ddhconnect::get_lovs()
httptest::stop_capturing()


# test for supporting function
res_checks <- all_checks[all_checks$node_type == "resource", ]

httptest::with_mock_api({
  test_that("Does not add check if public dataset", {
    metadata <- list("nid" = "007")
    metadata$field_wbddh_data_class$und[[1]]$tid <- "358"
    lovs <- ddhconnect::get_lovs()
    expect_equal(
      add_privacy_check(metadata, res_checks, lovs),
      res_checks[res_checks$func_names != "check_privacy_status", ]
    )
  })
})

httptest::with_mock_api({
  test_that("Adds check if not a public dataset", {
    metadata <- list("nid" = "007")
    metadata$field_wbddh_data_class$und[[1]]$tid <- "359"
    lovs <- ddhconnect::get_lovs()
    expect_equal(
      add_privacy_check(metadata, res_checks, lovs),
      res_checks
    )
  })
})

# test for check
test_that("Identifies public resource when dataset is not", {
  metadata <- list("nid" = "007")
  metadata$field_dataset_ref$und[[1]]$target_id <- "161"
  metadata$field_wbddh_data_class$und[[1]]$tid <- "358"
  expect_equal(
    check_privacy_status(metadata),
    list("resource", "007", "check_privacy_status", "FAIL", glue::glue("resource is public but dataset ", "161", " is not"))
  )
})

test_that("Passes if resource is not public", {
  metadata <- list("nid" = "007")
  metadata$field_dataset_ref$und[[1]]$target_id <- "161"
  metadata$field_wbddh_data_class$und[[1]]$tid <- "359"
  expect_equal(
    check_privacy_status(metadata),
    list("resource", "007", "check_privacy_status", "PASS", "both dataset and resource are not public")
  )
})

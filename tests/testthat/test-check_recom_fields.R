context("Tests check for missing recommended fields")

httptest::start_capturing(path = "./tests/testthat")
dkanr::dkanr_setup(
  url = "https://datacatalog.worldbank.org/",
  username = Sys.getenv("ddh_username"),
  password = Sys.getenv("ddh_password")
)
ddhconnect::get_metadata(nid = "140158")
ddhconnect::get_lovs()
httptest::stop_capturing()


httptest::with_mock_api({
  test_that("All recommended fields are populated", {
    metadata <- ddhconnect::get_metadata(nid = "140158")
    lovs <- ddhconnect::get_lovs()
    expect_equal(
      check_recom_fields(metadata, lovs),
      list("dataset", "140158", "check_recom_fields", "PASS", "No missing recommended fields")
    )
  })
})

httptest::with_mock_api({
  test_that("Correctly identifies missing recommended fields when empty string", {
    metadata <- ddhconnect::get_metadata(nid = "140158")
    metadata$body <- NA
    lovs <- ddhconnect::get_lovs()
    expect_equal(
      check_recom_fields(metadata, lovs),
      list("dataset", "140158", "check_recom_fields", "FAIL", glue::glue("Missing the following: ", "body"))
    )
  })
})

context("Tests check if resource file extension match")

# httptest::start_capturing(path = "./tests/testthat")
# dkanr::dkanr_setup(
#   url = "https://datacatalog.worldbank.org/",
#   username =  Sys.getenv("ddh_username"),
#   password =   Sys.getenv("ddh_password")
# )
# ddhconnect::get_metadata(nid = "93789")
# ddhconnect::get_lovs()
# httptest::stop_capturing()

httptest::with_mock_api({
  test_that("The resource is a external link", {
    metadata <- ddhconnect::get_metadata(nid = "93789")
    lovs <- ddhconnect::get_lovs()
    expect_equal(
      check_file_ext(metadata, lovs),
      list("resource", "93789", "check_file_ext", "PASS", "Resource is external link.")
    )
  })
})


httptest::with_mock_api({
  test_that("Resource file/link extension matched or no pre-existing requirement", {
    metadata <- ddhconnect::get_metadata(nid = "93789")
    metadata$field_link_api <- list()
    metadata$field_link_remote_file <- list()
    metadata$field_upload <- list('und'=list(list( 'uri' = "test_add_data.csv" )))
    metadata[['field_format']] <- list('und'=list(list('tid'= "14")))
    lovs <- ddhconnect::get_lovs()
    expect_equal(
      check_file_ext(metadata, lovs),
      list("resource", "93789", "check_file_ext", "PASS",  "Resource extension format matched/No pre-existing requirement on given format.")
    )
  })
})

httptest::with_mock_api({
  test_that("Resource link or file missing", {
    metadata <- ddhconnect::get_metadata(nid = "93789")
    metadata[c('field_link_api', 'field_link_remote_file', 'field_upload')] <- NA
    lovs <- ddhconnect::get_lovs()
    expect_equal(
      check_file_ext(metadata, lovs),
      list("resource", "93789", "check_file_ext", "FAIL", "Missing resource link or file.")
    )
  })
})


httptest::with_mock_api({
  test_that("Resource file/link extension blank", {
    metadata <- ddhconnect::get_metadata(nid = "93789")
    metadata$field_link_api <- list()
    metadata$field_link_remote_file <- list()
    metadata$field_upload <- list('und'=list(list( 'uri' = "https://geowb.worldbank.org/portal/sharing/servers/bb6bb3bb54c949c7ba2720d8ce6b33b6/rest/services/MYS_Demographics_and_Boundaries/MapServer")))
    metadata[['field_format']] <- list('und'=list(list('tid'= "14")))
    lovs <- ddhconnect::get_lovs()
    expect_equal(
      check_file_ext(metadata, lovs),
      list("resource", "93789", "check_file_ext", "FAIL",  glue::glue("The resource path is expected to take csv extension(s)"))
    )
  })
})

httptest::with_mock_api({
  test_that("Resource file/link extension do not match as required", {
    metadata <- ddhconnect::get_metadata(nid = "93789")
    metadata$field_link_api <- list()
    metadata$field_link_remote_file <- list()
    metadata$field_upload <- list('und'=list(list( 'uri' = "test_add_data.csv" )))
    metadata[['field_format']] <- list('und'=list(list('tid'= "1369")))
    lovs <- ddhconnect::get_lovs()
    expect_equal(
      check_file_ext(metadata, lovs),
      list("resource", "93789", "check_file_ext", "FAIL", glue::glue("The field_format's (GeoJSON) allowed types (geojson,json) do not match the resource's file ext (csv)"))
    )
  })
})

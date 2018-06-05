context("Tests supporting check for resource file extensions")

httptest::start_capturing(path = "./tests/testthat")
dkanr::dkanr_setup(
  url = "https://datacatalog.worldbank.org/",
  username = Sys.getenv("ddh_username"),
  password = Sys.getenv("ddh_password")
)
ddhconnect::get_lovs()
httptest::stop_capturing()

# constants
lovs <- ddhconnect::get_lovs()

# Test get_field_format()
test_that("Maps existing tids to list value names", {
  expect_equal(get_field_format(list("field_format" = "14"), lovs), "CSV")
  expect_equal(get_field_format(list("field_format" = "1194"), lovs), "EXCEL")
  expect_equal(get_field_format(list("field_format" = "659"), lovs), "CSV ZIP")
  expect_equal(get_field_format(list("field_format" = "17"), lovs), "VECTOR")
})

# Test get_allowed_ext()
test_that("Maps list value names to allowed file extensions", {
  expect_equal(get_allowed_ext("CSV"), "csv")
  expect_equal(get_allowed_ext("EXCEL"), c("xls", "xlsx", "ods"))
  expect_equal(get_allowed_ext("CSV ZIP"), "zip")
  expect_equal(get_allowed_ext("VECTOR"), "pdf")
  expect_equal(get_allowed_ext(NA), NA)
})

# Test get_file_ext()
test_that("Trims URL paths to file extensions", {
  expect_equal(get_file_ext("http://databank.worldbank.org/data/download/WDI_excel.zip"), "zip")
  expect_equal(get_file_ext("http://databank.worldbank.org/data/download/WDIrevisions.xls"), "xls")
})

test_that("Matches with upper/lower cases", {
  expect_equal(get_file_ext("abc.XLSX"), "xlsx")
  expect_equal(get_file_ext("abc.CSV"), "csv")
})


# Define constant values
# test_metadata <- list(
#   "field_format" = "14",
#   "field_upload" = "hi.org/bye.csv",
#   "field_link_api" = "hi.org/bye.csv",
#   "field_link_remote_file" = "hi.org/bye.csv"
# )

# Test logic in check_file_ext()
# test_that("Overall function works", {
#   expect_equal(check_file_ext(test_metadata))
# })
# TODO: try mocking with nodes 94974, 97633, 94457, 94668

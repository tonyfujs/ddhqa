context("Tests supporting check for resource file extensions")

# Define constant values
test_metadata <- list(
  "field_format" = "14",
  "field_upload" = "hi.org/bye.csv",
  "field_link_api" = "hi.org/bye.csv",
  "field_link_remote_file" = "hi.org/bye.csv"
)

# Test get_field_format()
test_that("Maps existing tids to allowed file exts", {
  expect_equal(get_field_format(list("field_format" = "14")), "csv")
  expect_equal(get_field_format(list("field_format" = "1194")), c("xls", "xlsx", "ods"))
  expect_equal(get_field_format(list("field_format" = "659")), "zip")
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

# Test logic in check_file_ext()
# test_that("", {
#   expect_equal()
# })


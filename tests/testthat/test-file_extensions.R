context("File extensions for resources links")

# Define constant values

# Test get_field_format
test_that("Maps existing tids to allowed file exts", {
  expect_equal(get_field_format(list("field_format" = "14")), "csv")
  expect_equal(get_field_format(list("field_format" = "1194")), c("xls", "xlsx", "ods"))
  expect_equal(get_field_format(list("field_format" = "659")), "zip")
})

test_that("Prints error if tids do not exist", {
})

# Test get_file_ext()
test_that("Trims URL paths to file extensions", {
  expect_equal(get_file_ext("http://databank.worldbank.org/data/download/WDI_excel.zip"), "zip")
  expect_equal(get_file_ext("http://databank.worldbank.org/data/download/WDIrevisions.xls"), "xls")
})

test_that("Matches with upper/lower cases", {
  expect_equal(get_file_ext("abc.XLSX"), "xlsx")
})


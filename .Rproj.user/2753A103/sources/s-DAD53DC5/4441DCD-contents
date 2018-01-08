context("File extensions for resources links")

# Define constant values

# Test return_file_ext()
test_that("Trims URL paths to file extensions", {
  expect_equal(return_file_ext("http://databank.worldbank.org/data/download/WDI_excel.zip"), "zip")
  expect_equal(return_file_ext("http://databank.worldbank.org/data/download/WDIrevisions.xls"), "xlsx")
})

test_that("Matches with upper/lower cases", {
  expect_equal(return_file_ext("abc.XLSX"), "xlsx")
})

# Test extract_file_path()
# need to build test JSON -> in data/test-resource_json
# extract the correct value from one of the machine names
# skips if there missing val

# Test extract_format() (same test as extract_file_path)

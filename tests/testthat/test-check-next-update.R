context("Tests check for next update date")

# checks for support functions
# will need to update checks depending current day
test_that("Creates correct date from list of months", {
  expect_equal(
    get_update_months("Jan, Apr, Jul, Sep", month.abb),
    as.Date("2018-04-01")
  )
  expect_equal(
    get_update_months("Jan,Apr,Jul,Sep", month.abb),
    as.Date("2018-04-01")
  )
  expect_equal(
    get_update_months("June, December", month.name),
    as.Date("2018-06-01")
  )
  expect_equal(
    get_update_months("June,December", month.name),
    as.Date("2018-06-01")
  )
})

test_that("Creates correct date from day dash month", {
  expect_equal(get_update_dash("25-Mar"), as.Date("2018-03-25"))
  expect_equal(get_update_dash("15-May"), as.Date("2018-05-15"))
  expect_equal(get_update_dash("01-Dec"), as.Date("2018-12-01"))
})

test_that("Creates correct date from a numerical value", {
  expect_equal(get_update_num("2016"), as.Date("2016-01-01"))
  expect_equal(get_update_num("2015"), as.Date("2015-01-01"))
  expect_equal(get_update_num("15"), as.Date("2018-05-15"))
  expect_equal(get_update_num("03"), as.Date("2018-06-03"))
})

# all test cases for update_schedule
nid <- "007"
modified <- "2018-02-20"

test_that("Handles update schedule", {
  update <- "5M"
  expect_equal(
    use_update_schedule(nid, update, modified),
    list("dataset", "007", "check_next_update", "PASS", glue::glue("The next expected update is ", "2018-07-20"))
  )
  update <- "1Y"
  expect_equal(
    use_update_schedule(nid, update, modified),
    list("dataset", "007", "check_next_update", "PASS", glue::glue("The next expected update is ", "2019-02-20"))
  )
  update <- "Jan, Apr, July, Sep"
  expect_equal(
    use_update_schedule(nid, update, modified),
    list("dataset", "007", "check_next_update", "FAIL", glue::glue("An update should have occurred on ", "2018-04-01"))
  )
  update <- "Jan,Apr,July,Sep"
  expect_equal(
    use_update_schedule(nid, update, modified),
    list("dataset", "007", "check_next_update", "FAIL", glue::glue("An update should have occurred on ", "2018-04-01"))
  )
  update <- "June, December"
  expect_equal(
    use_update_schedule(nid, update, modified),
    list("dataset", "007", "check_next_update", "FAIL", glue::glue("An update should have occurred on ", "2018-06-01"))
  )
  update <- "June,December"
  expect_equal(
    use_update_schedule(nid, update, modified),
    list("dataset", "007", "check_next_update", "FAIL", glue::glue("An update should have occurred on ", "2018-06-01"))
  )
  update <- "28"
  expect_equal(
    use_update_schedule(nid, update, modified),
    list("dataset", "007", "check_next_update", "FAIL", glue::glue("An update should have occurred on ", "2018-05-28"))
  )
  update <- "01"
  expect_equal(
    use_update_schedule(nid, update, modified),
    list("dataset", "007", "check_next_update", "FAIL", glue::glue("An update should have occurred on ", "2018-06-01"))
  )
  update <- "15-Mar"
  expect_equal(
    use_update_schedule(nid, update, modified),
    list("dataset", "007", "check_next_update", "FAIL", glue::glue("An update should have occurred on ", "2018-03-15"))
  )
  update <- "15-Dec"
  expect_equal(
    use_update_schedule(nid, update, modified),
    list("dataset", "007", "check_next_update", "PASS", glue::glue("The next expected update is ", "2018-12-15"))
  )
  update <- "2016"
  expect_equal(
    use_update_schedule(nid, update, modified),
    list("dataset", "007", "check_next_update", "PASS", "Updated on time")
  )
  update <- "2019"
  expect_equal(
    use_update_schedule(nid, update, modified),
    list("dataset", "007", "check_next_update", "PASS", glue::glue("The next expected update is ", "2019-01-01"))
  )
})

httptest::start_capturing(path = "./tests/testthat")
dkanr::dkanr_setup(
  url = "https://datacatalog.worldbank.org/",
  username = Sys.getenv("ddh_username"),
  password = Sys.getenv("ddh_password")
)
lovs <- ddhconnect::get_lovs()
httptest::stop_capturing()

# all test cases for update_frequency
# test against node 94562 and add capturing for lovs
metadata <- list("nid" = "007")
metadata$field_wbddh_update_frequency$id <- "531"
metadata$field_wbddh_next_expected_update$und[[1]]$value <- "2016-07-30 00:00:00"
metadata$field_wbddh_modified_date$und[[1]]$value <- "2017-06-14 00:00:00"
metadata$field_wbddh_update_schedule$und[[1]]$safe_value <- "February, June, August, November"
# format looks off
# metadata$field_wbddh_update_frequency$und
# metadata$field_wbddh_next_expected_update <- "2016-07-30 00:00:00"
# metadata$field_wbddh_modified_date$und[[1]]$value <- "2017-06-14 00:00:00"
# metadata$field_wbddh_update_schedule$und[[1]]$safe_value <- "February, June, August, November"
httptest::with_mock_api({
  test_that("Different vals for update freq work", {
    # Daily
    lovs <- ddhconnect::get_lovs()
    metadata$field_wbddh_update_frequency$und <- "516"
    expect_equal(
      check_next_update(metadata, lovs),
      list("dataset", "007", "check_next_update", "FAIL", "this dataset needs to be updated (daily update)")
    )
    # Weekly
    metadata$field_wbddh_update_frequency$und <- "521"
    expect_equal(
      check_next_update(metadata, lovs),
      list("dataset", "007", "check_next_update", "FAIL", "this dataset needs to be updated (weekly update)")
    )
    # No fixed schedule
    metadata$field_wbddh_update_frequency$und <- "526"
    expect_equal(
      check_next_update(metadata, lovs),
      list("dataset", "007", "check_next_update", "FAIL", "next expected update needs to be updated to a later date")
    )
    # No further updates planned
    metadata$field_wbddh_update_frequency$und <- "511"
    expect_equal(
      check_next_update(metadata, lovs),
      list("dataset", "007", "check_next_update", "FAIL", glue::glue("An update should have occurred on ", "2018-06-01"))
    )
    # Other
  })
})


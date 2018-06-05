context("Tests check for next update date")

# checks for support functions
# will need to update checks depending current day
test_that("Creates correct date from list of months", {
  expect_equal(
    get_update_months("Jan, Apr, July, Sep", month.abb),
    as.Date("2018-04-01")
  )
  expect_equal(
    get_update_months("Jan,Apr,July,Sep", month.abb),
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
    list("dataset", "007", "check_next_update", "FAIL", glue::glue("An update should have occurred on ", "2018-12-15"))
  )
  update <- "2016"
  expect_equal(
    use_update_schedule(nid, update, modified),
    "2016-01-01"
  )
  update <- "2019"
  expect_equal(
    use_update_schedule(nid, update, modified),
    "2016-01-01"
  )
})


# all test cases for update_frequency
metadata <- list("nid" = "007")
# Daily
metadata$field_wbddh_update_frequency <- "516"
# Weekly
metadata$field_wbddh_update_frequency <- "521"
# No fixed schedule
metadata$field_wbddh_update_frequency <- "526"
# No further updates planned
metadata$field_wbddh_update_frequency <- "511"
# Other

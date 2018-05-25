context("Tests check for embargo date")

test_that("Flags passed date", {
  metadata <- list("nid" = "007", "field_wbddh_ds_embargo_date" = "1997-06-26 00:00:00")
  expect_equal(
    check_embargo_date(metadata),
    list("dataset", "007", "check_missing", "FAIL", "embargo date has passed")
  )
})

test_that("Ok with empty date", {
  metadata <- list("nid" = "007", "field_wbddh_ds_embargo_date" = "")
  expect_equal(
    check_embargo_date(metadata),
    c("dataset", "007", "check_missing", "PASS", "there is no embargo date")
  )
})

test_that("Ok with future date", {
  metadata <- list("nid" = "007", "field_wbddh_ds_embargo_date" = "2027-06-26 00:00:00")
  expect_equal(
    check_embargo_date(metadata),
    c("dataset", "007", "check_missing", "PASS", "embargo date hasn't passed")
  )
})

#' check_next_update
#'
#' Check if update date has passed
#'
#' @param metadata_dataset list: object returned by get_metadata()
#' @param lovs dataframe: object returned by the get_lovs() function
#'
#' @return vector
#' @export
#'

# TODO: check node 94562, json structure is different than expected val, seems like legacy
# [ ] metadata_dataset[["field_wbddh_update_schedule"]]
# [ ] metadata_dataset[["field_wbddh_next_expected_update"]]
# [ ] metadata_dataset[["field_wbddh_modified_date"]]

check_next_update <- function(metadata_dataset,
                              lovs = ddhconnect::get_lovs()) {

  dataset_nid <- unlist(metadata_dataset[["nid"]], use.names = FALSE)
  update_freq <- unlist(metadata_dataset[["field_wbddh_update_frequency"]], use.names = FALSE)
  update_schedule <- unlist(metadata_dataset[["field_wbddh_update_schedule"]], use.names = FALSE)
  modified <- unlist(metadata_dataset[["field_wbddh_modified_date"]][["und"]][[1]][["value"]], use.names = FALSE)
  modified_date <- as.Date(modified)
  update <- unlist(metadata_dataset[["field_wbddh_next_expected_update"]][["und"]][[1]][["value"]], use.names = FALSE)
  update_date <- ifelse(!is_blank(update), as.Date(update), '')

  if (!is_blank(update_freq)) {

    update_freq_tid <- unlist(metadata_dataset[["field_wbddh_update_frequency"]][["und"]], use.names = FALSE)
    update_freq_ui <- lovs[lovs$tid == update_freq_tid & lovs$machine_name == "field_wbddh_update_frequency", ]$list_value_name

    if (update_freq_ui == "Daily") {

      if (modified_date == Sys.Date()) {
        out <- list("dataset", dataset_nid, "check_next_update", "PASS", "dataset is updated")
      } else {
        out <- list("dataset", dataset_nid, "check_next_update", "FAIL", "this dataset needs to be updated (daily update)")
      }

    } else if (update_freq_ui == "Weekly") {

      if ((Sys.Date() - modified_date) <= 7) {
        out <- list("dataset", dataset_nid, "check_next_update", "PASS", "dataset is updated")
      } else {
        out <- list("dataset", dataset_nid, "check_next_update", "FAIL", "this dataset needs to be updated (weekly update)")
      }

    } else if (update_freq_ui == "No fixed schedule") {

      update <- unlist(metadata_dataset[["field_wbddh_next_expected_update"]][["und"]][[1]][["value"]], use.names = FALSE)
      update_date <- as.Date(update)
      if (update_date > Sys.Date()) {
        out <- list("dataset", dataset_nid, "check_next_update", "PASS", "next expected update is before the last modified date")
      } else if (update_date > modified_date) {
        out <- list("dataset", dataset_nid, "check_next_update", "FAIL", "dataset needs to be updated")
      } else {
        out <- list("dataset", dataset_nid, "check_next_update", "FAIL", "next expected update needs to be updated to a later date")
      }

    } else {

      update_schedule <- unlist(metadata_dataset[["field_wbddh_update_schedule"]][["und"]][[1]][["safe_value"]], use.names = FALSE)
      out <- use_update_schedule(dataset_nid, update_schedule, modified_date)

    }

  } else if (!is_blank(update_date) | !is_blank(update_schedule)){
    out <- list("dataset", dataset_nid, "check_next_update", "FAIL", "missing value for update frequency")
  } else {
    out <- list("dataset", dataset_nid, "check_next_update", "PASS", "No update values to check")
  }
  return(out)
}



use_update_schedule <- function(dataset_nid, update_schedule, modified_date) {

  if (grepl("M$", update_schedule)) {
    month <- gsub("M", "",  update_schedule)
    expected_update <- as.Date(modified_date)
    lubridate::month(expected_update) <- lubridate::month(expected_update) + as.numeric(month)
  } else if (grepl("Y$", update_schedule)) {
    year <- as.numeric(gsub("Y", "",  update_schedule))
    expected_update <- as.Date(modified_date)
    lubridate::year(expected_update) <- lubridate::year(expected_update) + as.numeric(year)
  } else if (unlist(strsplit(update_schedule, ",|, "))[1] %in% month.abb) {
    expected_update <- get_update_months(update_schedule, month.abb)
  } else if (unlist(strsplit(update_schedule, ",|, "))[1] %in% month.name) {
    expected_update <- get_update_months(update_schedule, month.name)
  } else if (grepl("-", update_schedule)) {
    expected_update <- get_update_dash(update_schedule)
  } else if (!is.na(as.numeric(update_schedule))) {
    expected_update <- get_update_num(update_schedule)
  } else {
    expected_update <- NA
  }

  if (is.na(expected_update)) {
    out <- list("dataset", dataset_nid, "check_next_update", "FAIL", glue::glue("double check the update_schedule: update_schedule"))
  } else if (expected_update > Sys.Date()) {
    out <- list("dataset", dataset_nid, "check_next_update", "PASS", glue::glue("The next expected update is {expected_update}"))
  } else if (modified_date > expected_update) {
    out <- list("dataset", dataset_nid, "check_next_update", "PASS", "Updated on time")
  } else {
    out <- list("dataset", dataset_nid, "check_next_update", "FAIL", glue::glue("An update should have occurred on {expected_update}"))
  }

  return(out)
}

get_update_months <- function(update_schedule, month_lookup) {
  today <- Sys.Date()
  current_year <- lubridate::year(today)

  update_months <- unlist(strsplit(update_schedule, ",|, "))
  update_months_num <- match(tolower(update_months), tolower(month_lookup))

  dates <- lapply(update_months_num, function(x) lubridate::make_date(current_year, x, 1))
  dates <- c(do.call(c, dates), lubridate::make_date(current_year - 1, max(update_months_num), 1))

  past_dates <- dates[which(dates <= today)]
  out <- past_dates[which.min(abs(past_dates - today))]

  return(out)
}

get_update_dash <- function(update_schedule) {
  update_date <- unlist(strsplit(update_schedule, "-"))
  update_day <- update_date[1]
  update_month <- match(update_date[2], month.abb)
  current_year <- lubridate::year(Sys.Date())
  out <- lubridate::make_date(current_year, update_month, update_day)
  return(out)
}

get_update_num <- function(update_schedule) {
  today <- Sys.Date()
  if (nchar(update_schedule) == 2) {
    update_schedule <- as.numeric(update_schedule)
    out <- today
    if (update_schedule > lubridate::day(today)) {
      lubridate::month(out) <- lubridate::month(out) - 1
      lubridate::day(out) <- update_schedule
    } else {
      lubridate::day(out) <- update_schedule
    }
  } else {
    out <- lubridate::make_date(update_schedule, 1, 1)
  }
  return(out)
}

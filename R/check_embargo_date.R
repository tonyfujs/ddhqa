#' check_embargo_date
#'
#' Check if the dataset's embargo date has passed
#'
#' @param metadata_dataset list: object returned by get_metadata() on a dataset
#' @param lovs dataframe: object returned by the get_lovs() function
#'
#' @return list
#' @export
#'

check_embargo_date <- function(metadata_dataset) {

  embargo_date <- unlist(metadata_dataset$field_wbddh_ds_embargo_date, use.names = FALSE)
  embargo_date <- as.Date(embargo_date)
  dataset_nid <- unlist(metadata_dataset$nid, use.names = FALSE)

  if (is_blank(embargo_date)) {
    out <- list("dataset", dataset_nid, "check_embargo_date", "PASS", "there is no embargo date")
  } else if (embargo_date >= Sys.Date()) {
    out <- list("dataset", dataset_nid, "check_embargo_date", "FAIL", "embargo date has passed")
  } else {
    out <- list("dataset", dataset_nid, "check_embargo_date", "PASS", "embargo date hasn't passed")
  }

  return(out)

}

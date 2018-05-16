#' check_missing
#'
#' Check if the dataset is missing resources
#'
#' @param metadata_dataset list: output of get_metadata() for a dataset
#'
#' @return character vector
#' @export
#'

check_missing <- function(metadata_dataset) {

  dataset_nid <- unlist(metadata_dataset$nid, use.names = FALSE)
  resource_nids <- unlist(metadata_dataset$field_resources, use.names = FALSE)

  if (length(resource_nids) > 0) {
    out <- c("dataset", dataset_nid, "check_missing", "PASS", "has resources")
  } else {
    out <- c("dataset", dataset_nid, "check_missing", "FAIL", "needs resources")
  }

  return(out)
}

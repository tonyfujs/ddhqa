#' check_missing
#'
#' Get missing resources for all datasets
#'
#' @param metadata_dataset list: output of get_metadata() for a dataset
#'
#' @return character vector
#' @export
#'

check_missing <- function(metadata_dataset) {

  dataset_nid <- unname(unlist(metadata_dataset$nid))
  resource_nid <- get_resources_list(metadata_dataset)

  if (length(resource_nid) != 0) {
    out <- c(dataset_nid, "PASS", "has resources")
  } else {
    out <- c(dataset_nid, "FAIL", "missing resources")
  }

  return(out)
}

#' check_unpublished
#'
#' Get unpublished resources of one or all of the data types
#'
#' @param metadata_resource list: output of get_metadata() for a resource
#'
#' @return character vector
#' @export
#'

check_unpublished <- function(metadata_resource) {

  dataset_nid <- unname(unlist(metadata_resource$field_dataset_ref))
  resource_nid <- metadata_resource$nid
  resource_status <- unname(unlist(metadata_resource$status))

  if (resource_status == 1) {
    out <- c(dataset_nid, resource_nid, "PASS", "published")
  } else {
    out <- c(dataset_nid, resource_nid, "FAIL", "unpublished")
  }

  return(out)
}

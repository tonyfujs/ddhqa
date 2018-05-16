#' check_unpublished
#'
#' Check if resource is unpublished
#'
#' @param metadata_resource list: output of get_metadata() for a resource
#'
#' @return character vector
#' @export
#'

check_unpublished <- function(metadata_resource) {

  dataset_nid <- unlist(metadata_resource$field_dataset_ref, use.names = FALSE)
  resource_nid <- metadata_resource$nid
  resource_status <- unlist(metadata_resource$status, use.names = FALSE)

  if (resource_status == 1) {
    out <- c("resource", resource_nid, "check_unpublished", "PASS", "published")
  } else {
    out <- c("resource", resource_nid, "check_unpublished", "FAIL", glue("unpublished resource for dataset {dataset_nid}"))
  }

  return(out)
}

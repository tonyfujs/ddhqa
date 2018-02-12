#' check_unpublished
#'
#' Get unpublished resources of one or all of the data types
#'
#' @param datatype character: datatype of datasets for which to return the resource nids
#'
#' @return character vector
#' @export
#'

check_unpublished <- function(metadata_resource) {

  resource_status <- unname(unlist(metadata_resource$status))
  if (resource_status == 1) {
    out <- "published"
  } else {
    out <- "unpublished"
  }

  return(out)
}

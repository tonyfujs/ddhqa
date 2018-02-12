#' get_unpublished_resources
#'
#' Get unpublished resources of one or all of the data types
#'
#' @param datatype character: datatype of datasets for which to return the resource nids
#'
#' @return character vector
#' @export
#'

get_unpublished_resource_nids <-  function(datatype = 'Microdata') {
  out <- ddhconnect::get_datasets_list(datatype = datatype,
                                       root_url = dkanr::get_url(),
                                       credentials = list(cookie = dkanr::get_cookie(), token = dkanr::get_token()))
  dataset_nids <- out$nid
  dataset_resources <- purrr::map(dataset_nids,
                              function(x) ddhconnect::get_resource_nid(x,
                                                                       root_url = dkanr::get_url(),
                                                                       credentials = list(cookie = dkanr::get_cookie(), token = dkanr::get_token())))
  resource_nids <- unlist(dataset_resources)
  resource_statuss <- purrr::map(resource_nids,
                                 function(x) ddhconnect::get_metadata(x)$status)
  unpublished_inds <- which(resource_statuss == 0)
  unpublished_resource_nids <- resource_nids[unpublished_inds]
}

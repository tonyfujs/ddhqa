#' get_metadata_resources
#'
#' Gather all the metadata corresponding to every nid for resources
#'
#' @param metadata_datasets list: object returned by the get_metadata_datasets() function
#' @param credentials list: object returned by the get_credentials() function
#'
#' @return list
#' @export
#'

get_metadata_datasets <- function (metadata_datasets,
                                   credentials = list(cookie = dkanr::get_cookie(),
                                                      token = dkanr::get_token())){
  nid_resources <- get_resources_list(metadata_datasets)
  metadata_resources <- get_metadata_all(nid_resources, credentials)
  return(metadata_resources)
}

#' get_metadata_datasets
#'
#' Gather all the metadata corresponding to every nid for datasets
#'
#' @param data_type string: type of data to subset
#' @param credentials list: object returned by the get_credentials() function
#'
#' @return list
#' @export
#'

get_metadata_datasets <- function (data_type = "all",
                                   credentials = list(cookie = dkanr::get_cookie(),
                                                      token = dkanr::get_token())){
  datasets <- ddhconnect::get_datasets_list(data_type)
  metadata_datasets <- get_metadata_all(datasets, credentials)
  return(metadata_datasets)
}

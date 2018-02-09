#' get_resources_list()
#'
#' @param datasets_metadata character: output of get_metadata_datasets
#' @param est_count character: Approximate number of resources
#' @param credentials list: API authentication credentials
#'
#' @return numeric vector
#' @export
#'
#'


get_resources_list <- function(datasets_metadata,
                               rand_count = 100000,
                               credentials = list(cookie = dkanr::get_cookie(),
                                                  token = dkanr::get_token())){
  nid_resources <- vector("list", rand_count)
  count <- 1

  for (dataset in datasets_metadata){
    resources <- dataset$field_resources
    if (!is.null(resources$und$target_id)) {
      for (id in resources$und$target_id) {
        nid_resources[count] <- id
        count <- count + 1
      }
    }
  }

  out <- nid_resources[!sapply(nid_resources, is.null)]
  return(out)
}

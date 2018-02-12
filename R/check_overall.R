#' check_overall
#'
#' Overall automated quality checks for the Data Catalog
#'
#' @param metadata_datasets list: object returned by get_metadata_datasets
#' @param lovs dataframe: object returned by the get_lovs() function
#'
#' @return dataframe
#' @export
#'

# TODO add different checks as parameters
# TODO think about a good way to coallate results
check_overall <- function(nid_dataset,
                          # recom_fields = TRUE,
                          # file_ext = TRUE,
                          # file_links = TRUE,
                          resources_check = TRUE,
                          credentials = list(cookie = dkanr::get_cookie(),
                                             token = dkanr::get_token())) {

# quality checks for datasets
  dataset <- get_metadata(nid_dataset)
  check_recom_fields(dataset)

  # quality checks for resources
  if (resources_check) {
    nid_resources <- unname(unlist(dataset$field_resources))
    for (node in nid_resources) {

      tryCatch({
        resource <- get_metadata(nid = node)
      }, error = function(e) {
        cat("ERROR :", node, conditionMessage(e), "\n")
      })

      check_file_ext(resource)
      check_resource_links(resource)
      check_unpublished(resource)
    }
  }
  return("not sure yet")
}



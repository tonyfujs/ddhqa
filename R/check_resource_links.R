#' check_resource_links
#'
#' Check for 404 errors in resource links
#'
#' @param metadata_resources list: object returned by get_metadata_resources or get_metadata_all
#'
#' @return dataframe
#' @export
#'

check_resource_links <- function(metadata_resource) {
  # TODO need to include? # set_config(config(ssl_verifypeer = 0L, timeout_ms = 2000))
  url_results <- resource %>%
                 extract_file_path %>%
                 inspect_url

  return(url_results)
}


inspect_url <- function(path) {

  resp <- tryCatch({
    httr::GET(path)
  }, error = function(e) {
    cat("ERROR :", conditionMessage(e), "\n")
  })

  if (is.null(resp)) {
    code <- -1
  } else {
    code <- httr::status_code(resp)
  }

  return(c(code, path))
}

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
  url <- dkanr::get_resource_url(metadata_resource)

  resp <- tryCatch({
    httr::GET(url)
  }, error = function(e) {
    cat("ERROR :", conditionMessage(e), "\n")
  })

  if (is.null(resp)) {
    code <- "No response"
  } else {
    code <- httr::status_code(resp)
  }

  return(c(code, url))
}

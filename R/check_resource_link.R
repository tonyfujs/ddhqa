#' check_resource_link
#'
#' Check for 404 errors in resource links
#'
#' @param metadata_resources list: object returned by get_metadata_resources or get_metadata_all
#'
#' @return dataframe
#' @export
#'

# TODO need to include? # set_config(config(ssl_verifypeer = 0L, timeout_ms = 2000))

check_resource_link <- function(metadata_resource) {

  url <- dkanr::get_resource_url(metadata_resource)
  resource_nid <- unlist(metadata_resource$nid, use.names = FALSE)

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

  if (grepl("geowb.worldbank.org", url)) {
    out <- list("resource", resource_nid, "check_resource_links", "PASS", glue::glue("{code}, esri link, you're good to go"))
  } else if (code == "200") {
    out <- list("resource", resource_nid, "check_resource_links", "PASS", glue::glue("{code}, you're good to go"))
  } else {
    out <- list("resource", resource_nid, "check_resource_links", "FAIL", glue::glue("{code}, check the link"))
  }

  return(out)
}

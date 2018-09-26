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

  lovs = ddhconnect::get_lovs()

  url <- dkanr::get_resource_url(metadata_resource)
  resource_nid <- unlist(metadata_resource$nid, use.names = FALSE)

  resp <- tryCatch({
    httr::GET(url)
  }, error = function(e) {
    cat("ERROR :", conditionMessage(e), "\n")
  })

  if (!is.null(resp)) {
    code <- httr::status_code(resp)
  } else {
    code <- "No response"
  }

  if (length(url)>0){

    if (grepl("geowb.worldbank.org", url)) {
      out <- list("resource", resource_nid, "check_resource_links", "PASS", glue::glue("{code}, esri link, you're good to go"))
    } else if (code == "200") {
      out <- list("resource", resource_nid, "check_resource_links", "PASS", glue::glue("{code}, you're good to go"))
    } else {
      out <- list("resource", resource_nid, "check_resource_links", "FAIL", glue::glue("{code}, check the link"))
    }
  } else{

    class <- unlist(metadata_resource$field_wbddh_data_class, use.names = FALSE)

    if (class %in% lovs[grep('Confidential', lovs$list_value_name, ignore.case = T),]$tid ) {
      out <- list("resource", resource_nid, "check_resource_links", "PASS", glue::glue("{code},confidential resource, you're good to go"))
    } else {
      out <- list("resource", resource_nid, "check_resource_links", "FAIL", glue::glue("{code}, check the link"))
    }
  }

  return(out)
}

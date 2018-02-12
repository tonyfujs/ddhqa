#' publish_resources
#'
#' Publish resources from the list of node ids
#'
#' @param resource_nids character vector: list of nodes to gather data for
#'
#' @return character
#' @export
#'

publish_resources <-  function(resource_nids) {
  json_template <- list()
  json_template$workflow_status <- 'published'
  body <- jsonlite::toJSON(json_template, auto_unbox = TRUE)
  purrr::map(resource_nids, function(x) ddhconnect::update_resource(nid = x, body = body))
}

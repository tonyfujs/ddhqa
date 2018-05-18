#' check_overall
#'
#' Overall automated quality check for each dataset
#'
#' @param nid_dataset string: node id for a dataset
#' @param checks: string: the check you want to perform
#' @param lovs dataframe: object returned by the get_lovs() function
#'
#' @return dataframe
#' @export
#'

#TODO: better handle the checks parameter
# c("check_file_ext", "check_missing", "check_recom_fields", "check_resource_link", "check_unpublished" )
#TODO: break function into smaller pieces and switch to lapply(funcs, function(f) f(dataset))
#TODO: review R inferno chapter on growing objects, rbind definitely not the best idea

check_overall <- function(nid_dataset,
                          checks = "all",
                          credentials = list(cookie = dkanr::get_cookie(),
                                             token = dkanr::get_token())) {

  out <- data.frame(
    node_type = character(),
    node_id = character(),
    check_name = character(),
    message = character(),
    status = character(),
    stringsAsFactors = FALSE
  )

  #subset the required number of checks based on input
  if (checks != "all") {
    all_checks <- subset(all_checks, func_names %in% checks)
  }

  # get dataset metadata
  tryCatch({
    dataset <- get_metadata(nid_dataset)
  }, error = function(e) {
    cat("ERROR :", node, conditionMessage(e), "\n")
  })

  data_checks <- subset(all_checks, node_type == "dataset")
  for (data_check in data_checks$func_names) {
    result <- get(data_check)(dataset)
    out <- rbind(out, result, stringsAsFactors=FALSE)
  }

  # quality checks for resources
  nid_resources <- unlist(dataset$field_resources, use.names = FALSE)
  res_checks <- subset(all_checks, node_type == "resource")
  if (nrow(resource_checks) > 0) {
    for (i in 1:length(nid_resources)) {
      nid_res <- nid_resources[[i]]

      # get resource metadata
      tryCatch({
        resource <- get_metadata(nid = nid_res)
      }, error = function(e) {
        cat("ERROR :", node, conditionMessage(e), "\n")
      })

      for (res_check in res_checks$func_names) {
        result <- get(res_check)(resource)
        out <- rbind(out, result, stringsAsFactors=FALSE)
      }
    }
  }

  names(out) <- c("node_type", "node_id", "check_name", "message", "status")
  return(out)
}

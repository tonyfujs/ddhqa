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

check_overall <- function(nid_dataset,
                          checks = "all",
                          credentials = list(cookie = dkanr::get_cookie(),
                                             token = dkanr::get_token())) {

  #subset the required number of checks based on input
  if (checks != "all") {
    all_checks <- dplyr::filter(all_checks, func_names %in% checks)
  }

  # get dataset metadata
  tryCatch({
    dataset <- ddhconnect::get_metadata(nid_dataset)
  }, error = function(e) {
    cat("ERROR :", node, conditionMessage(e), "\n")
  })

  data_checks <- dplyr::filter(all_checks, node_type == "dataset")
  data_out <- vector(mode = "list", length = nrow(data_checks))
  for (i in seq_along(data_checks$func_names)) {
    result <- get(data_checks$func_names[i])(dataset)
    names(result) <- c("node_type", "node_id", "check_name", "status", "message")
    data_out[[i]] <- result
  }
  data_out <- dplyr::bind_rows(data_out)

  # quality checks for resources
  nid_resources <- unlist(dataset$field_resources, use.names = FALSE)

  res_checks <- dplyr::filter(all_checks, node_type == "resource")
  res_checks <- add_privacy_check(dataset, res_checks)
  res_out <- vector(mode = "list", length = length(nid_resources))
  if (nrow(res_checks) > 0) {
    for (i in seq_along(nid_resources)) {
      nid_res <- nid_resources[[i]]

      # get resource metadata
      tryCatch({
        resource <- ddhconnect::get_metadata(nid = nid_res)
      }, error = function(e) {
        cat("ERROR :", node, conditionMessage(e), "\n")
      })

      temp <- vector(mode = "list", length = nrow(res_checks))
      for (j in seq_along(res_checks$func_names)) {
        result <- get(res_checks$func_names[j])(resource)
        names(result) <- c("node_type", "node_id", "check_name", "status", "message")
        temp[[j]] <- result
      }
      temp <- dplyr::bind_rows(temp)
      res_out[[i]] <- temp
    }
    res_out <- dplyr::bind_rows(res_out)
  }

  out <- dplyr::bind_rows(data_out, res_out)

  return(out)
}

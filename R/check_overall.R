#' check_overall
#'
#' Overall automated quality check for each dataset
#'
#' @param nid_dataset string: node id for a dataset
#' @param checks:
#' @param lovs dataframe: object returned by the get_lovs() function
#'
#' @return dataframe
#' @export
#'
# c("check_file_ext", "check_missing", "check_recom_fields", "check_resource_link", "check_unpublished" )
#TODO: fix this
#TODO: add different checks as parameters using function names maybe?
#TODO: think about a good way to coallate results, currently using rbind around line 65
#TODO: add the nids to the results
#TODO: break function into smaller pieces

check_overall <- function(nid_dataset,
                          checks = "all",
                          credentials = list(cookie = dkanr::get_cookie(),
                                             token = dkanr::get_token())) {

  # subset the required number of checks based on input
  if (checks == "all") {
    fun_todo <- mutate(all_checks, funcs = lapply(func_names, get))
  } else {
    less_checks <- subset(all_checks, parameter %in% checks)
    fun_todo <- mutate(less_checks, funcs = lapply(func_names, get))
  }

  # get dataset metadata
  tryCatch({
    dataset <- get_metadata(nid_dataset)
  }, error = function(e) {
    cat("ERROR :", node, conditionMessage(e), "\n")
  })

  # quality checks for datasets
  out <- fun_todo %>%
    subset(type == "dataset") %>%
    mutate(result = lapply(funcs, function(f) f(dataset)))

  # quality checks for resources
  nid_resources <- ddhconnect::get_resource_nids(dataset)
  resource_check <- fun_todo %>% subset(type == "resource")
  if (nrow(resource_check) > 0) {
    for (i in 1:length(nid_resources)) {
      nid_res <- nid_resources[[i]]

      # get resource metadata
      tryCatch({
        resource <- get_metadata(nid = nid_res)
      }, error = function(e) {
        cat("ERROR :", node, conditionMessage(e), "\n")
      })

      out_resource <- fun_todo %>%
        subset(type == "resource") %>%
        mutate(result = lapply(funcs, function(f) f(resource)))

      out <- rbind(out, out_resource)
    }
  }

  clean_out <- subset(out, select = -funcs)
  return(out)
}

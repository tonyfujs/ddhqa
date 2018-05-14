#' check_overall
#'
#' Overall automated quality check for each dataset
#'
#' @param nid_data string: node id for a dataset
#' @param lovs dataframe: object returned by the get_lovs() function
#'
#' @return dataframe
#' @export
#'

#TODO: check programming with dplyr
#TODO: add different checks as parameters
#TODO: think about a good way to coallate results, currently using rbind on line 65
#TODO: initialize output dataframe based on number of checks?
#TODO: add the nids to the results

check_overall <- function(nid_data,
                          checks = c("recommended", "exts", "links", "unpublished"),
                          credentials = list(cookie = dkanr::get_cookie(),
                                             token = dkanr::get_token())) {

  # subset the required number of checks based on input
  fun_todo <- all_checks %>%
    subset(parameter %in% checks) %>%
    mutate(funcs = lapply(func_names, get))

  tryCatch({
    dataset <- get_metadata(nid_data)
  }, error = function(e) {
    cat("ERROR :", node, conditionMessage(e), "\n")
  })

  # quality checks for datasets
  out <- fun_todo %>%
    subset(type == "dataset") %>%
    mutate(result = lapply(funcs, function(f) f(dataset)),
           nid = nid_data)

  # quality checks for resources
  nid_resources <- unname(unlist(dataset$field_resources))
  resource_check <- fun_todo %>% subset(type == "resource")
  if (nrow(resource_check) > 0) {
    for (i in 1:length(nid_resources)) {
      nid_res <- nid_resources[[i]]

      tryCatch({
        resource <- get_metadata(nid = nid_res)
      }, error = function(e) {
        cat("ERROR :", node, conditionMessage(e), "\n")
      })

      out_resource <- fun_todo %>%
        subset(type == "resource") %>%
        mutate(result = lapply(funcs, function(f) f(resource)),
               nid = nid_res) %>%

      out <- rbind(out, out_resource)
    }
  }

  clean_out <- out %>% select(-funcs)
  return(out)
}
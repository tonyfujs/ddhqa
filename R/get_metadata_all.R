#' get_metadata_all
#'
#' Gather all the metadata corresponding to every nid passed, used for count_machine_names
#'
#' @param nids list: list of nids
#' @param credentials list: object returned by the get_credentials() function
#'
#' @return list
#' @export
#'
# TODO do not want to keep all datasets in memory
get_metadata_all <- function(nids = ddhconnect::get_datasets_list("all"),
                             credentials = list(cookie = dkanr::get_cookie(),
                                               token = dkanr::get_token())) {

 metadata_all <- vector("list", length(nids))
 names(metadata_all) <- nids

 for (node in nids){
   tryCatch({
     metadata_node <- ddhconnect::get_metadata(nid = node,
                                   credentials = credentials)
     metadata_all[[node]] <- metadata_node
   }, error = function(e) {
     cat("ERROR :", node, conditionMessage(e), "\n")
     })
 }

 return(metadata_all)
}

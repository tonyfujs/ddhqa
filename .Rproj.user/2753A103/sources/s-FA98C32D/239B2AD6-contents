# not sure if want to publish, might make sense for only testing a subset of nodes

get_metadata_all <- function(nids,
                             credentials = list(cookie = dkanr::get_cookie(),
                                               token = dkanr::get_token())) {

 metadata_all <- vector("list", length(nids))
 names(metadata_all) <- nids

 for (node in nids){
   tryCatch({
     metadata_node <- get_metadata(nid = node,
                                   credentials = credentials)
     metadata_all[[node]] <- metadata_node
   }, error=function(e){cat("ERROR :", node, conditionMessage(e), "\n")})
 }

 return(metadata_all)
}

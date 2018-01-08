get_resources_list <- function(metadata_datasets,
                               credentials = list(cookie = dkanr::get_cookie(),
                                                  token = dkanr::get_token())){

  nid_resources <- vector("list", length(metadata_datasets))
  # nid_datasets <- vector("list", length(datasets_metadata))
  count <- 1

  for(dataset in datasets_metadata){
    resources <- dataset$field_resources
    if(!is.null(resources$und$target_id)){
      for(id in resources$und$target_id){
        nid_resources[count] <- id
        # nid_datasets[count] <- dataset$nid
        count <- count + 1
      }
    }
  }

  nid_resources <- cbind(nid_resources, nid_datasets)
  #resources[!apply(resources == "", 1, all),]
  return(nid_resources)
}

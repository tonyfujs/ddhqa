library(ddhqa)
library(ddhconnect)

#microdata_datasets <- get_datasets_list("Microdata")

# current.n <- length(microdata_datasets$nid) + 10
current.n <- 10
out <- data.frame(
  dataset = character(current.n),
  resource = character(current.n),
  status = character(current.n),
  desc = character(current.n),
  stringsAsFactors = FALSE
  )

i <- 1
#setdiff(microdata_datasets$nid, resource_checks$dataset)
for (dataset_nid in microdata_datasets$nid) {

  tryCatch({
    resource_nid <- get_resource_nid(dataset_nid)

    tryCatch({
      resource <- get_metadata(resource_nid)
      out[i, ] <- check_unpublished(resource)
      i <- i + 1

    }, error = function(e) {
      print(c(dataset_nid, resource_nid))
      cat("RESOURCE ERROR :", conditionMessage(e), "\n")
    })

  }, error = function(e) {
    print(dataset_nid)
    cat("DATASET ERROR :", conditionMessage(e), "\n")
  })

  }


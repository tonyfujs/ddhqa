#' check_recom_fields
#'
#' Check which recommended fields are missing
#'
#' @param metadata_dataset list: object returned by get_metadata()
#' @param lovs dataframe: object returned by the get_lovs() function
#'
#' @return vector
#' @export
#'

#TODO: check recommended_fields table
# [ ] don't really need the recommended column, but double check
# [ ] find the collections machine name or remove it from the table

check_recom_fields <- function(metadata_dataset,
                               lovs = ddhconnect::get_lovs()) {
  dataset_nid <- unlist(metadata_dataset$nid, use.names = FALSE)
  tid_type <- unlist(metadata_dataset$field_wbddh_data_type, use.names = FALSE)
  ui_name <- lovs[lovs$tid == tid_type, ]$list_value_name


  rec_fields <- recommended_fields[recommended_fields$data_type == ui_name, ]
  rec_fields <- rec_fields[!is.na(rec_fields$recommended), ]
  clean_rec_fields <- rec_fields[["machine_name"]]

  populated_fields <- metadata_dataset[lapply(metadata_dataset, length) > 0]
  missing_rec_fields <- setdiff(clean_rec_fields, names(populated_fields))

  if (length(missing_rec_fields) > 0) {
    str <- paste0(missing_rec_fields, collapse = ", ")
    out <- list("dataset", dataset_nid, "check_recom_fields", "FAIL", glue::glue("Missing the following: {str}"))
  } else {
    out <- list("dataset", dataset_nid, "check_recom_fields", "PASS", "No missing recommended fields")
  }

  return(out)
}

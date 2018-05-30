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

check_recom_fields <- function(metadata_dataset,
                               lovs = ddhconnect::get_lovs()) {
  dataset_nid <- unlist(metadata_dataset[["nid"]], use.names = FALSE)
  tid_type <- unlist(metadata_dataset[["field_wbddh_data_type"]], use.names = FALSE)
  ui_name <- lovs[lovs$tid == tid_type, ]$list_value_name


  rec_fields <- recommended_fields[recommended_fields$data_type == ui_name, ]
  rec_fields <- rec_fields[!is.na(rec_fields$recommended), ]
  clean_rec_fields <- rec_fields[["machine_name"]]

  # only include if matches conditions
  if (length(metadata_dataset[["field_wbddh_data_class"]]) > 1) {
    data_class <- unlist(metadata_dataset[["field_wbddh_data_class"]], use.names = FALSE)
    ui_data_class <- lovs[lovs$tid == data_class, ]$list_value_name
    if (ui_data_class %in% c("Public", "Not Specified")) {
      clean_rec_fields <- c(clean_rec_fields, "field_exception_s_")
    }
  }

  if (length(metadata_dataset[["field_license_wbddh"]]) > 1) {
    license <- unlist(metadata_dataset[["field_license_wbddh"]], use.names = FALSE)
    ui_license <- lovs[lovs$tid == license, ]$list_value_name
    if (ui_data_class == "Custom License") {
      clean_rec_fields <- c(clean_rec_fields, "field_wbddh_type_of_license")
    }
  }

  populated_fields <- metadata_dataset[lapply(metadata_dataset, is_blank) == FALSE]
  missing_rec_fields <- setdiff(clean_rec_fields, names(populated_fields))

  if (length(missing_rec_fields) > 0) {
    str <- paste0(missing_rec_fields, collapse = ", ")
    out <- list("dataset", dataset_nid, "check_recom_fields", "FAIL", glue::glue("Missing the following: {str}"))
  } else {
    out <- list("dataset", dataset_nid, "check_recom_fields", "PASS", "No missing recommended fields")
  }

  return(out)
}


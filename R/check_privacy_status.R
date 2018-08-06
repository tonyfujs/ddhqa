#' check_privacy_status
#'
#' Check if the confidential/official use only dataset has public resources
#'
#' @param metadata_resource list: output of get_metadata() for a resource
#'
#' @return character vector
#' @export
#'

check_privacy_status <- function(metadata_resource,
                                 lovs = ddhconnect::get_lovs()) {

  dataset_nid <- unlist(metadata_resource[["field_dataset_ref"]], use.names = FALSE)
  resource_nid <- unlist(metadata_resource[["nid"]], use.names = FALSE)
  data_class_tid <- unlist(metadata_resource[["field_wbddh_data_class"]], use.names = FALSE)
  data_class_ui <- lovs[lovs$tid == data_class_tid & lovs$machine_name == "field_wbddh_data_class", ]$list_value_name

  if (data_class_ui == "Public") {
    out <- list("resource", resource_nid, "check_privacy_status", "FAIL", glue::glue("resource is public but dataset {dataset_nid} is not"))
  } else {
    out <- list("resource", resource_nid, "check_privacy_status", "PASS", "both dataset and resource are not public")
  }

  return(out)
}



add_privacy_check <- function(metadata_dataset, res_checks,
                              lovs = ddhconnect::get_lovs()) {

  data_class_tid <- unlist(metadata_dataset[["field_wbddh_data_class"]], use.names = FALSE)
  data_class_ui <- lovs[lovs$tid == data_class_tid & lovs$machine_name == "field_wbddh_data_class", ]$list_value_name

  if (data_class_ui == "Public") {
    res_checks <- res_checks[res_checks$func_names != "check_privacy_status", ]
  }

  return(res_checks)
}

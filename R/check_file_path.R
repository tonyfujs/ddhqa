#' check_file_path
#'
#' Check if the resources have the correct path based on data classification
#'
#' @param metadata_resource list: output of get_metadata() for a resource
#'
#' @return character vector
#' @export
#'

check_file_path <- function(metadata_resource) {

  resource_nid <- unlist(metadata_resource[["nid"]], use.names = FALSE)
  upload_path <- unlist(metadata[["field_upload"]], use.names = FALSE)

  data_class_tid <- unlist(metadata_resource[["field_wbddh_data_class"]], use.names = FALSE)
  data_class_ui <- lovs[lovs$tid == data_class_tid & lovs$machine_name == "field_wbddh_data_class", ]$list_value_name

  if (!is_blank(upload_path)) {
    if (data_class_ui == "Public" & grepl("ddhfiles/public", upload_path)) {
      out <- list("resource", resource_nid, "check_file_path", "PASS", "resource is public and file path is public")
    } else if (data_class_ui == "Public" & grepl("ddhfiles/internal", upload_path)) {
      out <- list("resource", resource_nid, "check_file_path", "FAIL", "resource is public but file path is internal")
    } else if (data_class_ui != "Public" & grepl("ddhfiles/public", upload_path)) {
      out <- list("resource", resource_nid, "check_file_path", "FAIL", "resource is not public but file path is public")
    } else {
      out <- list("resource", resource_nid, "check_file_path", "FAIL", "resource is internal and file path is internal")
    }
  } else {
    out <- list("resource", resource_nid, "check_file_path", "PASS", "resource is not uploaded")
  }

  return(out)
}

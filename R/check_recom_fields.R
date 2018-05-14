#' check_recom_fields
#'
#' Check which recommended fields are missing
#'
#' @param metadata list: object returned by get_metadata()
#' @param lovs dataframe: object returned by the get_lovs() function
#'
#' @return vector
#' @export
#'

# TODO check programming with dplyr
check_recom_fields <- function(metadata,
                               lovs = ddhconnect::get_lovs()) {

  populated <- metadata[lapply(metadata, length) > 0]

  tid_type <- unlist(metadata$field_wbddh_data_type, use.names = FALSE)
  ui_name <- subset(lovs, tid == tid_type, select = list_value_name)

  missing_rec_fields <- subset(recommended_fields,
                               data_type == unlist(ui_name, use.names = FALSE)) %>%
                        na.omit(.) %>%
                        filter(!(machine_name %in% names(populated))) %>%
                        pull(machine_name) %>%
                        unique(.)

  if (length(missing_rec_fields) > 0) {
    str <- paste0(missing_rec_fields, collapse = ", ")
    out <- c("FAIL", glue("Missing the following: {str}"))
  } else {
    out <- c("PASS", "No missing recommended")
  }
  return(out)
}

#' check_recom_fields
#'
#' Check which recommended fields are missing
#'
#' @param datasets list: object returned by get_metadata()
#' @param lovs dataframe: object returned by the get_lovs() function
#'
#' @return vector
#' @export
#'

check_recom_fields <- function (dataset) {
  # TODO maybe include error handler if not dataset tryCatch({})
  # TODO not sure if the value is actually populated, currently only checking if the machine name exists
  lovs <- ddhconnect::get_lovs()
  dataset_df <- dataset %>%
                format_metadata_df()

  # TODO might want to create a function to convert the tid to ui name
  tid_type <- unname(unlist(dataset$field_wbddh_data_type))
  ui_name <- lovs %>%
             subset(tid == tid_type, select = list_value_name) %>%
             pull(list_value_name)

  missing_rec_fields <- recommended_fields %>%
                        filter(!is.na(recommended)) %>%
                        subset(data_type == ui_name) %>%
                        filter(!(machine_name %in% dataset_df$metadata_names)) %>%
                        pull(machine_name)

  out <- unique(na.omit(missing_rec_fields))
  return(out)
}

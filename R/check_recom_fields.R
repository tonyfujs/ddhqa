#' check_recom_fields
#'
#' Check if the resource file extension matches the field_form value
#'
#' @param metadata_datasets list: object returned by get_metadata_datasets
#' @param lovs dataframe: object returned by the get_lovs() function
#'
#' @return dataframe
#' @export
#'

# might want to consider something that look as recommended resources?
# but there are not any recommended for resournces
check_recom_fields <- function (metadata_datasets,
                                credentials = list(cookie = dkanr::get_cookie(),
                                                   token = dkanr::get_token())) {
  #tryCatch({})
  # include error handler if not dataset
  lovs <- ddhconnect::get_lovs(credentials = credentials)

  for (dataset in metadata_datasets) {
    dataset_df <- format_metadata_df(dataset)

    # in case the tids change in the future
    # might want to create a function
    tid <- unlist(dataset$field_wbddh_data_type)
    ui_name <- lovs[lovs$tid == tid, ]$list_value_name
    rec_fields <- recommended_fields[which(recommended_fields$data_type == ui_name &
                                            !is.na(recommended_fields$recommended)), ]$machine_name

    # identify missing recommended_fields
    mia_rec <- rec_fields[!(rec_fields %in% dataset_df$metadata_names)]

    # need to extract populated
    exist_rec <- dataset_df[which(dataset_df$metadata_names %in% rec_fields), ]
    used_rec <- unique(na.omit(exist_rec)$metadata_names)
    unused_rec <- exist_rec[!(exist_rec$metadata_names %in% used_rec), ]$metadata_names

    print(dataset$nid)
    print(mia_rec[!is.na(mia_rec)])
    print(unused_rec)
  }
  return()
}

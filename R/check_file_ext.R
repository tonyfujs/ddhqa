#' check_file_ext
#'
#' Check if the resource file extension matches the field_form value
#'
#' @param metadata_resource dataframe: object returned by assemble_file_ext
#' @param lovs dataframe: object returned by the get_lovs() function
#'
#' @return dataframe
#' @export
#'

check_file_ext <- function(resource_nid){

  metadata_resource <- get_metadata(resource_nid)
  lovs <- ddhconnect::get_lovs()
  field_format <- extract_field_format(metadata_resource, lovs)

  file_ext <- tryCatch(
    {
      metadata_resource %>%
      extract_file_path %>%
      return_file_ext %>%
      verify_valid_ext
    },
    error = function(e){
      return(NA)
    }
  )

  if ((is.na(file_ext) & is.na(field_format)) | identical(tolower(field_format), tolower(file_ext))){
    return(c(field_format = field_format, file_ext = file_ext, match = TRUE))
  }
  else{
    return(c(field_format = field_format, file_ext = file_ext, match = FALSE))
  }
}

extract_field_format <- function(metadata_resource,
                                 lovs = ddhconnect::get_lovs()) {
  field_format_tid <- unlist(metadata_resource$field_format)

  # grab list value name based on tid
  if (!is_blank(field_format_tid)) {
    expected_val <- lovs %>%
                    subset(tid == field_format_tid) %>%
                    pull(list_value_name)
  } else {
    expected_val <- NA
  }

  return(expected_val)
}

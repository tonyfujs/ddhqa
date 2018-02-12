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

check_file_ext <- function(metadata_resource,
                           lovs = ddhconnect::get_lovs()){

  field_format <- extract_field_format(resource, lovs)
  file_ext <- resource %>%
                extract_file_path %>%
                return_file_ext %>%
                verify_valid_ext
  # TODO: might need a separate check to verified the type is allowed? build into verify_valid_ext

  if (identical(field_format, file_ext)) {
    out <- "CONGRATSSSSSSSSS~~~(^-^*~~~)"
  } else {
    out <- "TRY AGAIN"
  }

  return(out)
}

extract_field_format <- function(metadata_resource,
                                 lovs = ddhconnect::get_lovs()) {
  field_format_tid <- unlist(resource$field_format)

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

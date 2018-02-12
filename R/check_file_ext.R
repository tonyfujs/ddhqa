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
                           lookup = lookup_ext_to_form){

  field_format <- extract_field_format(resource, lovs)
  file_ext <- resource %>%
               extract_file_path %>%
               return_file_ext %>%
               verify_valid_ext
  return(c(field_format, file_ext))

  # populated_format <- results[!is.na(results$field_formats), ]
  #
  # temp <- merge(populated_format,
  #               lookup_ext_to_form,
  #               by.x = "field_formats",
  #               by.y = "list_value_name",
  #               all.x = TRUE)
  #
  # temp$matching <- mapply(grepl,
  #                         pattern = temp$allowed_type,
  #                         x = temp$file_exts)
  #
  # not_matching <- temp[which(temp$matching == 0 | is.na(temp$matching)), ]
  #
  # fields <- c("resource_nid", "field_formats", "allowed_type", "file_exts")
  # clean_not_matching <- not_matching[fields]
  # return(clean_not_matching)
}

extract_field_format <- function(resource, lovs) {
  field_format_tid <- unlist(resource$field_format)

  # grab list value name based on tid
  if (!is.null(field_format_tid)) {
    expected_val <- lovs[which(lovs$tid == field_format_tid), ]$list_value_name
  } else {
    expected_val <- NA
  }

  return(expected_val)
}

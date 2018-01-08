extract_field_format <- function(resource, lovs) {
  field_format_tid <- unlist(resource$field_format)

  # grab list value name based on tid
  if (!is.null(field_format_tid)) {
    expected_val <- lovs[which(lovs$tid == field_format_tid),]$list_value_name
  } else {
    expected_val <- NA
  }

  return(expected_val)
}

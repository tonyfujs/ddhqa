get_field_format <- function(metadata_resource,
                             lovs = ddhconnect::get_lovs()) {

  format_tid <- unlist(metadata_resource[["field_format"]], use.names = FALSE)
  out <- NA

  if (!is_blank(format_tid)) {
    field_format <- lovs[lovs$tid == format_tid & lovs$machine_name == "field_format", ]$list_value_name
    out <- unlist(field_format, use.names = FALSE)
  }

  return(out)
}


get_allowed_ext <- function(field_format) {

  if (!is_blank(field_format)){
    allowed <- lookup_ext_to_form[lookup_ext_to_form$list_value_name == field_format, ]$allowed_exts
    temp <- strsplit(unlist(allowed, use.names = FALSE), split = "\\|")
    out <- unlist(temp)
  } else {
    out <- NA
  }

  return(out)
}

get_file_ext <- function(file_path) {
  if (is.null(file_path)) {
    file_ext <- NA
  } else {
    file_name <- basename(file_path)
    file_ext <- tolower(tools::file_ext(file_name))
  }
  return(file_ext)
}

is_blank <- function(input){
  return(gtools::invalid(input) || all(input == ""))
}

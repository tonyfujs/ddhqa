get_field_format <- function(metadata_resource,
                             lovs = ddhconnect::get_lovs()) {

  format_tid <- unname(unlist(metadata_resource[["field_format"]]))
  out <- NA

  if (!is_blank(format_tid)) {
    temp <- subset(lovs, tid == format_tid, select = list_value_name)
    field_format <- unname(unlist(temp))

    if (!is.na(field_format)){
      allowed <- subset(lookup_ext_to_form,
                        list_value_name == field_format,
                        select = allowed_exts)
      temp <- strsplit(unname(unlist(allowed)),
                       split = "\\|")
      out <- unlist(temp)
    }
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

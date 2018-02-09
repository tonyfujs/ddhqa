extract_file_path <- function(resource) {
  # keeping field_link_api because some of those are file extensions
  loc_potential <- c(resource$field_link_api$und[[1]]$url,
                     resource$field_link_remote_file$und[[1]]$url,
                     resource$field_upload$und[[1]]$uri)

  # find the file path, should only be one per resource
  # need to add a check to make sure this is true

  # find a better way to extract
  loc_file <- loc_potential %>%
    unlist() %>%
    unname()

  if (is.null(loc_file)) {
    loc_file <- NA
  }

  return(loc_file)
}

# get the file extension from a file path
return_file_ext <- function(file_path) {
  if (is.null(file_path)) {
    file_ext <- NA
  } else {
    file_name <- basename(file_path)
    file_ext <- tolower(tools::file_ext(file_name))
  }
  return(file_ext)
}

# remove na and keep valid file extensions
verify_valid_ext <- function(file_ext) {
  if (file_ext %in% valid_file_ext$file_ext || is.na(file_ext)) {
    ext <- file_ext
  } else {
    ext <- ""
  }
  return(ext)
}

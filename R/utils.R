extract_file_path <- function(resource) {
  # keeping field_link_api because some of those are file extensions
  loc_potential <- c(resource$field_link_api$und[[1]]$url,
                     resource$field_link_remote_file$und[[1]]$url,
                     resource$field_upload$und[[1]]$uri)

  loc_file <- unname(unlist(loc_potential))
  if (is.null(loc_file)) {loc_file <- NA}
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

# TODO really need to rename this
# TODO improve performance, extremely slow
format_metadata_df <- function(metadata) {
  # flatten json
  metadata_vals <- unlist(metadata)
  # convert all empty string to missing vals
  metadata_vals[metadata_vals == ""] <- NA
  # extract machine names, remove nested information
  metadata_names <- trim_machine_names(metadata_vals)
  metadata_df <- data.frame(metadata_names, metadata_vals)
  return(metadata_df)
}

# TODO extract machine names, remove nested information after using unlist
# trim_machine_names <- function (metadata) {
#   machine_names <- names(metadata) %>%
#                   # as.character %>%
#                    strsplit(".", fixed = TRUE) %>%
#                    purrr::map_chr(c(1))
#   machine_names <- as.character(machine_names)
#   return(machine_names)
# }

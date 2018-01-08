#' check_file_ext.R
#'
#' Check if the resource file extension matches the field_form value
#'
#' @param metadata_resources list: object returned by get_metadata_resources
#' @param credentials list: object returned by the get_credentials() function
#'
#' @return dataframe
#' @export
#'

check_file_ext <- function(metadata_resources,
                           lovs = ddhconnect::get_lovs()){

  field_formats <- vector(mode = "character", length(metadata_resources))
  file_exts <- vector(mode = "character", length(metadata_resources))

  for (i in 1:length(metadata_resources)){
    resource <- metadata_resources[[i]]

    field_formats[i] <- extract_format(resource, lovs)
    file_exts[i] <- resource %>%
                      extract_file_path %>%
                      return_file_ext
  }

  ## MEERA, THIS IS SPARK NOTATION (>X_X<)
  # field_formats <- metadata_resources %>%
  #   extract_format(lovs)
  #
  # file_exts <- metadata_resources %>%
  #   extract_file_path %>%
  #   return_file_ext
  #   check_valid_file_ext

  results <- data.frame(resource_nid = names(metadata_resources),
                   field_formats,
                   file_exts)
  return(results)
}

###### NOTES NOTES NOTES ###############
# LOCATIONS WHERE THE LINK COULD BE (do not need link_api)
# field_link_api$und[[1]]$url, will probably remove
# field_link_remote_file$und[[1]]$url (not sure)
# field_upload$und[[1]]$url

# functions in order of appearance hahah

# might be a null value?
# do not want to call get_lovs for every resource
extract_format <- function(resource, lovs) {
  field_format_tid <- unlist(resource$field_format)
  if (!is.null(field_format_tid)) {
    expected_val <- lovs[which(lovs$tid == field_format_tid),]$list_value_name
  } else {
    expected_val <- NA
  }
  return(expected_val)
}

# extract metadata for those fields
extract_file_path <- function(resource) {
  # keeping field_link_api because some of those are file extensions
  loc_potential <- c(resource$field_link_api$und[[1]]$url,
                     resource$field_link_remote_file$und[[1]]$url,
                     resource$field_upload$und[[1]]$url)

  # find a better way to extract
  loc_file <- loc_potential %>%
    unlist()
  return(loc_file)
}

# running into issues of null paths, are these fields not required? is one of the three req?
# trims paths to just the file extension
return_file_ext <- function(file_path) {
  if (is.null(file_path)) {
    file_ext <- NA
  } else {
    file_name = basename(file_path)
    file_ext = tolower(tools::file_ext(file_name))
  }
  return(file_ext)
}

# valid_file_ext is saved data
# omits non file type strings (.com/.io)
# might not need this if only focusig on field_formats
# check_valid_file_ext <- function(file_ext) {
#   return(grep(file_ext, valid_file_ext, ignore.case = TRUE))
# }

# function to compare results
matching_vals <- function() {
  matching <- grep(val, file_ext, ignore.case = TRUE)
  print(matching)
  if (matching == 0) {
    print(nid, file_ext, expected_val)
  }
  return(c(expected_val, file_ext))
}

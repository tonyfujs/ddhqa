# really need to rename this
# improve performance, extremely slow
format_metadata_df <- function(metadata) {
  # flatten json
  metadata_vals <- unlist(metadata)
  # convert all empty string to missing vals
  metadata_vals[metadata_vals==""] <- NA
  # extract machine names, remove nested information
  metadata_names <- trim_machine_names(metadata_vals)
  metadata_df <- data.frame(metadata_names, metadata_vals)
  return(metadata_df)
}

# extract machine names, remove nested information after using unlist
trim_machine_names <- function (metadata_df) {
  machine_names <- names(metadata_df) %>%
    as.character %>%
    strsplit(".",fixed = TRUE) %>%
    purrr::map_chr(c(1))
  machine_names <- as.character(machine_names)
  return(machine_names)
}

#' assemble_file_ext.R
#'
#' Gather the file extension and the field_form value
#'
#' @param metadata_resources list: object returned by get_metadata_resources
#' @param lovs dataframe: object returned by the get_lovs() function
#'
#' @return dataframe
#' @export
#'

assemble_file_ext <- function(metadata_resources,
                           lovs = ddhconnect::get_lovs()){

  field_formats <- vector(mode = "character", length(metadata_resources))
  file_exts <- vector(mode = "character", length(metadata_resources))

  for (i in 1:length(metadata_resources)){
    resource <- metadata_resources[[i]]

    field_formats[i] <- extract_field_format(resource, lovs)
    file_exts[i] <- resource %>%
                      extract_file_path %>%
                      return_file_ext %>%
                      verify_valid_ext
  }

  results <- data.frame(resource_nid = names(metadata_resources),
                   field_formats,
                   file_exts)
  return(results)
}

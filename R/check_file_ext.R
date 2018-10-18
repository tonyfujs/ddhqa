#' check_file_ext
#'
#' Check if the resource file extension matches the expected field_form value
#'
#' @param metadata_resource named vector: object returned by get_metadata() on a resource
#' @param lovs dataframe: object returned by the get_lovs() function
#'
#' @return dataframe
#' @export
#'

#TODO: Confirm logic for file extension check
#TODO: Handle file extensions for external links
#[ ] only select if populated in one of the following fields; `field_upload, field_link_api, field_link_remote_file`
#[ ] deal with html in extension `is_blank(field_format) & !is_blank(file_ext)`

# r_nid 93789
# r_nid 100912

check_file_ext <- function(metadata_resource,
                           lovs = ddhconnect::get_lovs()) {

  resource_nid <- unlist(metadata_resource[["nid"]], use.names = FALSE)

  # Check the resource bucket
  buckets <- metadata_resource[c('field_link_api', 'field_link_remote_file', 'field_upload')]
  bucket <- names(which(!sapply(buckets, is_blank), arr.ind = TRUE)) # Extract the bucket where resource lies

  if (!length(bucket)>0){ # No Resource link/file

    out <- list("resource", resource_nid, "check_file_ext", "FAIL", "Missing resource link or file.")

  } else if (bucket == 'field_link_api') { # Resource is an external link
    out <- list("resource", resource_nid, "check_file_ext", "PASS", "Resource is external link.")
  } else {

  field_format <- get_field_format(metadata_resource, lovs)
  allowed_ext <- get_allowed_ext(field_format) # allowed ext for given field_fomat not listed in the look-up table, could be residual.
  allowed_collapsed <- glue::glue_collapse(allowed_ext, sep = ",")

  path <- dkanr::get_resource_url(metadata_resource)
  file_ext <- get_file_ext(path)

  # if (is_blank(allowed_ext) & is_blank(file_ext)) {
  #   out <- list("resource", resource_nid, "check_file_ext", "PASS", "Both are blank")
  # } else if (is_blank(allowed_ext) & !is_blank(file_ext)) {
  #   out <- list("resource", resource_nid, "check_file_ext", "FAIL", glue::glue("The field_format is missing, value should take a {file_ext} extenstion"))
  # } else if (!is_blank(allowed_ext) & is_blank(file_ext)) {
  #   out <- list("resource", resource_nid, "check_file_ext", "FAIL", glue::glue("The resource path is expected to take {allowed_collapsed} extension(s)"))
  # } else if (file_ext == allowed_ext | file_ext %in% allowed_ext) {
  #   out <- list("resource", resource_nid, "check_file_ext", "PASS", glue::glue("The field_format's ({field_format}) allowed types match the resource's file ext ({file_ext})"))
  # } else {
  #   out <- list("resource", resource_nid, "check_file_ext", "FAIL", glue::glue("The field_format's ({field_format}) allowed types ({allowed_collapsed}) do not match the resource's file ext ({file_ext})"))
  # }

 if (!is_blank(allowed_ext) & is_blank(file_ext)) {

    out <- list("resource", resource_nid, "check_file_ext", "FAIL",
                glue::glue("The resource path is expected to take {allowed_collapsed} extension(s)"))

  } else if (!is_blank(allowed_ext) & !is_blank(file_ext) & !(file_ext %in% allowed_ext)) {

    out <- list("resource", resource_nid, "check_file_ext", "FAIL",
                glue::glue("The field_format's ({field_format}) allowed types ({allowed_collapsed}) do not match the resource's file ext ({file_ext})"))
  } else  {

    out <- list("resource", resource_nid, "check_file_ext", "PASS", "Resource extension format matched/No pre-existing requirement on given format.")
  }
}

  return(out)
}

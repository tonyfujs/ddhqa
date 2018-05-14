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
                           lovs = ddhconnect::get_lovs()){

  field_format <- get_field_format(metadata_resource, lovs)

  url <- dkanr::get_resource_url(metadata_resource)
  file_ext <- get_file_ext(url)

  # TODO confirm logic
  if (is_blank(field_format) & !is_blank(file_ext)) {
    out <- c("FAIL", glue("The field_format is missing, value should take a {file_ext} extenstion"))
  } else if (!is_blank(field_format) & is_blank(file_ext)) {
    out <- c("FAIL", glue("The resource path is expected to take {field_format}"))
  } else if (file_ext %in% field_format | file_ext == field_format) {
    out <- c("PASS", glue("The field_format ({field_format}) matches the resource's file ext ({file_ext})"))
  } else {
    out <- c("PASS", "Both are blank so it should be ok? ")
  }

  return(out)
}

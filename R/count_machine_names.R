#' count_machine_names
#'
#' View the number of times a machine name is used in the metadata. Keeps count of used and missing
#'
#' @param all_metadata list: object returned by get_all_metadata
#' @param credentials list: object returned by the get_credentials() function
#'
#' @return dataframe
#' @export
#'

# need to optimize performance, initialize list/vector if possible, start with an empty list?
# need to test on resources, currently pulls only from datasets

count_machine_names <- function(all_data_fields){
    empty_fields = c()
    used_fields = c()
    for (metadata in all_data_fields){
      print(metadata$nid)

      # flatten json
      metadata_df <- unlist(metadata)

      # convert all empty string to missing vals
      metadata_df[metadata_df==""] <- NA

      # extract machine names, remove nested information
      machine_names <- names(metadata_df) %>%
        as.character %>%
        strsplit(".",fixed = TRUE) %>%
        map_chr(c(1))

      names(metadata_df) = machine_names
      exists = unique(names(na.omit(metadata_df)))
      contain_na = unique(names(metadata_df[is.na(metadata_df)]))
      # since json contains multiple keys per machine name
      missing = setdiff(contain_na, exists)

      empty_fields = c(empty_fields, missing)
      used_fields = c(used_fields, exists)
   }

   empty_fields_df = as.data.frame(table(empty_fields))
   used_fields_df = as.data.frame(table(empty_fields))
   final_df = merge(x = used_fields_df, y = empty_fields_df, by.x = "used_fields", by.y = "empty_fields", all = TRUE)
   return(final_df)
}
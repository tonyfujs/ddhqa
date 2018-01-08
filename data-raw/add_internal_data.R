# STEP 1: Get data --------------------------------------------------------

# all the valid options for uploading files as of 01/05/2017
valid_file_ext_df <- readr::read_csv("./data-raw/valid_file_ext.csv",
                                    col_types = readr::cols(
                                      file_ext = readr::col_character()
                                    ))
valid_file_ext <- valid_file_ext_df$file_ext

# STEP 4: Save table -------------------------------------------------------
devtools::use_data(valid_file_ext,
                   overwrite = TRUE)

# STEP 1: Get data --------------------------------------------------------

# all the valid options for uploading files as of 01/05/2017
valid_file_ext <- readr::read_csv("./data-raw/valid_file_exts.csv",
                                  col_types = readr::cols(
                                    file_ext = readr::col_character()
                                  ))

lookup_ext_to_form <- readr::read_csv("./data-raw/lookup_ext_to_form.csv")


recommended_fields <- readr::read_csv("./data-raw/recommended_fields.csv")


# STEP 4: Save table -------------------------------------------------------
devtools::use_data(valid_file_ext,
                   lookup_ext_to_form,
                   recommended_fields,
                   overwrite = TRUE)

#_________________________________________
# data
#_________________________________________

# all the valid options for uploading files as of 01/05/2017
valid_file_ext <- readr::read_csv("./data-raw/valid_file_exts.csv",
                                  col_types = readr::cols(
                                    file_ext = readr::col_character()
                                  ))

lookup_ext_to_form <- readr::read_csv("./data-raw/lookup_ext_to_form.csv")


recommended_fields <- readr::read_csv("./data-raw/recommended_fields.csv")

all_checks <- readr::read_csv("./data-raw/all_checks.csv")

#_________________________________________
# save
#_________________________________________
devtools::use_data(valid_file_ext,
                   lookup_ext_to_form,
                   recommended_fields,
                   all_checks,
                   #internal = TRUE,
                   overwrite = TRUE)

# library(httr)
#
# df <- read.csv("./output/resource_metadata.csv", stringsAsFactors = FALSE)
#
#
# # Concatenate URLs ---------------------------------------------------------------
#
# z <- df[(!(is.na(df$link_api)) & !(is.na(df$upload)) & !(is.na(df$source_url))),] #confirm that there is no data that is filled for the three fields together
# df$url <- paste(df$link_api, df$upload, df$source_url)
#
# for ( i in 1:nrow(df)) {
#   df$url[i] <- gsub("NA", '', df$url[i], fixed = TRUE)
#   df$url[i] <- gsub(" ", '', df$url[i], fixed = TRUE)
# }
#
#
# # HTTR Broken Link Test ---------------------------------------------------
#
# #df <- df[sample(1:nrow(df), size = 20), ]
#
# set_config(config(ssl_verifypeer = 0L, timeout_ms = 2000)) #This disables SSL verification, because it produces errors when the link is actually working, more here: https://stackoverflow.com/questions/40397932/r-peer-certificate-cannot-be-authenticated-with-given-ca-certificates-windows
#
# out_df <- data.frame()
# for (i in df$url) {
#   print(i)
#   resp <- tryCatch({
#     httr::GET(i)
#   }, error = function(e) {
#     NULL
#   })
#   if (is.null(resp)) {
#     httpcode <- -1
#     httpstatus <- "Connection error"
#   } else {
#     httpcode <- status_code(resp)
#     httpstatus <- http_status(resp)$reason
#   }
#   row <- data.frame(url=i, code=httpcode, status=httpstatus)
#   out_df <- rbind(out_df, row)
# }
#
# summary(out_df)

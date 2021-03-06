% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_metadata_all.R
\name{get_metadata_all}
\alias{get_metadata_all}
\title{get_metadata_all}
\usage{
get_metadata_all(nids = ddhconnect::get_datasets_list("all"),
  credentials = list(cookie = dkanr::get_cookie(), token =
  dkanr::get_token()))
}
\arguments{
\item{nids}{list: list of nids}

\item{credentials}{list: object returned by the get_credentials() function}
}
\value{
list
}
\description{
Gather all the metadata corresponding to every nid passed, used for count_machine_names
}
